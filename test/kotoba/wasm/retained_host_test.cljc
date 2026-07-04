(ns kotoba.wasm.retained-host-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.wasm.abi :as abi]
            [kotoba.wasm.host.retained :as retained]))

(def ops
  (:ops
   (abi/encode-batch
    [[:dom/create-element 1 :main]
     [:dom/set-root 1]
     [:dom/set-attr 1 :style/width 240]
     [:dom/create-element 2 :button]
     [:dom/set-attr 2 :id "run"]
     [:dom/set-attr 2 :style/background "#112233"]
     [:dom/add-event-listener 2 :click 99]
     [:dom/add-event-listener 2 :key-down 100]
     [:dom/create-text 3 "Run"]
     [:dom/append-child 2 3]
     [:dom/append-child 1 2]])))

(defn state []
  (reduce retained/apply-op
          (merge retained/base-state {:width 320})
          ops))

(deftest retained-state-applies-abi-ops
  (let [s (state)]
    (is (= 1 (:root s)))
    (is (= :main (get-in s [:nodes 1 :tag])))
    (is (= [2] (get-in s [:nodes 1 :children])))
    (is (= "run" (get-in s [:nodes 2 :attrs :id])))
    (is (= 99 (get-in s [:listeners 2 :click])))
    (is (= 100 (get-in s [:listeners 2 :key-down])))))

(deftest retained-node-tree-keeps-listeners-for-layout
  (let [tree (retained/node-tree (state) 1)
        button (first (:children tree))]
    (is (= :button (:tag button)))
    (is (= #{:click :key-down} (set (:listeners button))))
    (is (= ["Run"] (:children button)))))

(deftest retained-draw-ops-and-hit-test-are-deterministic
  (let [s (retained/with-draw-ops (state))
        button-node (some #(when (and (= :node (:draw/op %))
                                      (= :button (:tag %))) %)
                          (:draw-ops s))
        hit (retained/hit-test s (+ (:x button-node) 1) (+ (:y button-node) 1) :click)]
    (testing "style attrs flow into draw ops"
      (is (= 240 (:w (some #(when (and (= :node (:draw/op %))
                                       (= :main (:tag %))) %)
                           (:draw-ops s)))))
      (is (= "#112233" (:color (some #(when (and (= :rect (:draw/op %))
                                                (= :button (:tag %))) %)
                                    (:draw-ops s))))))
    (testing "hit-test maps pointer coordinates back to handler id"
      (is (= {:target 2 :handler 99} hit)))
    (testing "wrong event type does not hit"
      (is (nil? (retained/hit-test s (:x button-node) (:y button-node) :input))))))

(deftest retained-host-builds-pointer-and-focused-events
  (let [s (retained/with-draw-ops (state))
        button-node (some #(when (and (= :node (:draw/op %))
                                      (= :button (:tag %))) %)
                          (:draw-ops s))
        x (+ (:x button-node) 1)
        y (+ (:y button-node) 1)
        event (retained/hit-event s x y :click)]
    (testing "hit events carry handler, target, event name, and coordinates"
      (is (= {:handler 99 :target 2 :name :click :x x :y y} event)))
    (testing "queue-hit-event also establishes focus for keyboard input"
      (let [s (retained/queue-hit-event s x y :click)
            key-event (retained/focused-event s :key-down {:key "Enter"})]
        (is (= 2 (:focus s)))
        (is (= {:handler 100 :target 2 :name :key-down :key "Enter"} key-event))))
    (testing "queue-focused-event is a no-op without a focused listener"
      (let [s (retained/queue-focused-event s :input {:value "abc"})]
        (is (empty? (:events s)))))))

;; ---- optional real text-measurement plumbing (retained/draw-ops'
;; measure-text arg -> cssom.layout/draw-ops' :measure-text theme key) ----
;;
;; kotoba.wasm.host.webgl/webgpu's render! already holds a real Canvas 2D
;; `text-ctx` at the exact point it calls retained/draw-ops (used to
;; actually paint text with a real proportional system font), so it can
;; pass a (fn [text font-size] width-in-px) built from that context's
;; real `measureText` straight through -- see those namespaces'
;; measure-text-fn. This proves that plumbing genuinely reaches
;; cssom.layout's word-wrap (not merely threaded through and dropped),
;; using a fake stand-in for a real Canvas measureText (an honest
;; substitution for the real browser API this JVM test environment has
;; no access to, not a mock of the feature under test).

(def ^:private long-text-ops
  (:ops
   (abi/encode-batch
    [[:dom/create-element 1 :main]
     [:dom/set-root 1]
     [:dom/set-attr 1 :style/width 108]
     [:dom/create-text 2 "WWWWW WWWWW"]
     [:dom/append-child 1 2]])))

(defn- long-text-state []
  (reduce retained/apply-op
          (merge retained/base-state {:width 108})
          long-text-ops))

(defn- fake-proportional-measure
  "A FAKE (fn [text font-size] width-in-px), standing in for a real
   browser's CanvasRenderingContext2D.measureText the same way
   kotoba.wasm.host.webgl/webgpu's measure-text-fn wraps a real one --
   'W' measures much wider per character than the rest, unlike
   cssom.layout's built-in per-character approximation, which cannot
   distinguish characters at all."
  [text _font-size]
  (reduce + 0 (map (fn [c] (if (= c \W) 16 (if (= c \space) 6 8))) text)))

(deftest retained-draw-ops-without-measure-text-is-unchanged
  ;; No measure-text arg (both the 1-arg call, and with-draw-ops' own
  ;; 1-arg call) -- the exact same output as before this arg existed.
  (let [s (long-text-state)
        ops-1arg (retained/draw-ops s)
        ops-nil (retained/draw-ops s nil)
        text-ops (filterv #(= :text (:draw/op %)) ops-1arg)]
    (is (= ops-1arg ops-nil))
    ;; cssom.layout's default char-w approximation doesn't distinguish
    ;; 'W' from any other character, so this 11-char, 2-word string
    ;; comfortably fits on one line at width 108 (100px content-w).
    (is (= ["WWWWW WWWWW"] (mapv :text text-ops)))))

(deftest retained-draw-ops-with-measure-text-genuinely-changes-wrapping
  (let [s (long-text-state)
        ops (retained/draw-ops s fake-proportional-measure)
        text-ops (filterv #(= :text (:draw/op %)) ops)]
    ;; With the real (fake, in-test) proportional measurement consulted,
    ;; "WWWWW WWWWW" (166px measured) no longer fits the same 100px
    ;; content-w and wraps onto two lines -- a genuinely different wrap
    ;; decision than the unmeasured case above for the exact same input.
    (is (= ["WWWWW" "WWWWW"] (mapv :text text-ops)))))

(deftest with-draw-ops-threads-measure-text-through-too
  (let [s (retained/with-draw-ops (long-text-state) fake-proportional-measure)
        text-ops (filterv #(= :text (:draw/op %)) (:draw-ops s))]
    (is (= ["WWWWW" "WWWWW"] (mapv :text text-ops)))))

(deftest retained-event-queue-is-fifo
  (let [s (-> retained/base-state
              (retained/enqueue-event {:handler 1})
              (retained/enqueue-event {:handler 2}))
        [a s] (retained/poll-event s)
        [b s] (retained/poll-event s)
        [c s] (retained/poll-event s)]
    (is (= {:handler 1} a))
    (is (= {:handler 2} b))
    (is (nil? c))
    (is (empty? (:events s)))))
