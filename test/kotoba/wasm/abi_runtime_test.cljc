(ns kotoba.wasm.abi-runtime-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.wasm.abi :as abi]
            [kotoba.wasm.dom :as dom]
            [kotoba.wasm.host :as host]
            [kotoba.wasm.host.retained :as retained]
            [cssom.layout :as layout]
            [kotoba.wasm.runtime :as runtime]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(defn reset-counter! [n]
  (rf/clear!)
  (rf/reg-event-db :init (fn [_ _] {:count n :events []}))
  (rf/reg-event-db :inc (fn [db [_ event]]
                          (-> db
                              (update :count inc)
                              (update :events conj event))))
  (rf/reg-sub :count (fn [db _] (:count db)))
  (rf/reg-sub :events (fn [db _] (:events db)))
  (rf/dispatch-sync [:init]))

(defn reset-input-capture! []
  (rf/clear!)
  (rf/reg-event-db :init-input (fn [_ _] {:value "" :events []}))
  (rf/reg-event-db :capture-input
                   (fn [db [_ event]]
                     (-> db
                         (assoc :value (:value event))
                         (update :events conj event))))
  (rf/reg-event-db :capture-key
                   (fn [db [_ event]]
                     (update db :events conj event)))
  (rf/reg-sub :input-value (fn [db _] (:value db)))
  (rf/reg-sub :events (fn [db _] (:events db)))
  (rf/dispatch-sync [:init-input]))

(defn rich-counter []
  [:main#root.shell
   [:h1 "Counter"]
   (for [label ["a" "b"]]
     [:span.item label])
   nil
   false
   [:button#inc.primary.large
    {:class [:hot "wide"]
     :style {:padding "12px" :width "120px" :background "#334455"}
     :on-click (fn [event] (rf/dispatch [:inc event]))}
    "inc"]
   [:span.value @(rf/subscribe [:count])]])

(defn input-capture []
  [:main
   [:input#name
    {:value @(rf/subscribe [:input-value])
     :on-input (fn [event] (rf/dispatch [:capture-input event]))
     :on-key-down (fn [event] (rf/dispatch [:capture-key event]))}]
   [:span.value @(rf/subscribe [:input-value])]])

(deftest reagent-compatible-shorthand-and-seq-children
  (reset-counter! 2)
  (let [{:keys [document handlers]} (r/render [rich-counter])
        root (dom/node document (:root document))
        button-id (->> (:nodes document)
                       (some (fn [[id node]]
                               (when (= "inc" (get-in node [:attrs :id])) id))))
        button (dom/node document button-id)]
    (testing "tag shorthand maps to tag/id/class attrs"
      (is (= :main (:tag root)))
      (is (= "root" (get-in root [:attrs :id])))
      (is (= "shell" (get-in root [:attrs :class]))))
    (testing "class attrs merge with shorthand classes"
      (is (= "primary large hot wide" (get-in button [:attrs :class]))))
    (testing "style map becomes namespaced style attributes for host ABI"
      (is (= "12px" (get-in button [:attrs :style/padding])))
      (is (= "120px" (get-in button [:attrs :style/width]))))
    (testing "lazy child seqs are flattened and nil/false children are ignored"
      (is (= "Counterabinc2" (dom/text-content document))))
    (is (= 1 (count handlers)))))

(deftest abi-batch-validation-covers-invalid-inputs
  (is (= {:abi/version 1
          :ops [{:op :create-element :id 1 :tag "div"}
                {:op :set-attr :id 1 :namespace "style" :name "width" :value "12"}]}
         (abi/encode-batch [[:dom/create-element 1 :div]
                            [:dom/set-attr 1 :style/width 12]])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unsupported"
                        (abi/validate-batch {:abi/version 999 :ops []})))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid create-element"
                        (abi/validate-batch {:abi/version 1
                                             :ops [{:op :create-element :id "bad" :tag "div"}]})))
  (doseq [[message op] [["Invalid create-element" [:dom/create-element 1 nil]]
                        ["Invalid set-attr" [:dom/set-attr 1 nil "x"]]
                        ["Invalid add-event-listener" [:dom/add-event-listener 1 nil 9]]
                        ["Invalid add-event-listener" [:dom/add-event-listener 1 :click nil]]
                        ["Invalid append-child" [:dom/append-child 1 nil]]
                        ["Invalid remove-children" [:dom/remove-children nil]]
                        ["Invalid remove-child" [:dom/remove-child nil 2]]
                        ["Invalid remove-child" [:dom/remove-child 1 nil]]
                        ["Invalid insert-before" [:dom/insert-before nil 2 3]]
                        ["Invalid insert-before" [:dom/insert-before 1 nil 3]]
                        ["Invalid insert-before" [:dom/insert-before 1 2 "bad"]]]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo (re-pattern message)
                          (abi/validate-batch (abi/encode-batch [op])))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown"
                        (abi/encode-batch [[:dom/nope 1]]))))

(deftest abi-batch-validation-accepts-real-string-handler-ids
  ;; The confirmed repro, found via live-Chrome verification of a genuinely
  ;; unrelated fix (kotoba.wasm.dom's multi-listener bridging): the real
  ;; QuickJS webapi shim's own __kotobaHandlerId() (quickjs_wasm.cljc)
  ;; generates STRING handler-ids ("handler-1", "handler-2", ...), and the
  ;; window/document global-listener path uses its own string scheme
  ;; ("global-" + target + "-" + eventType) -- but validate-batch's
  ;; :add-event-listener/:dispatch-event cases required `(int? handler)`,
  ;; so ANY real script calling addEventListener() crashed the whole
  ;; commit with "Invalid add-event-listener op" the instant a real batch
  ;; reached this validation -- a pre-existing, previously-latent bug
  ;; nothing had exercised end to end before (host/commit! is the real
  ;; commit path every real host, including the live demo, uses).
  (is (= "handler-1"
         (:handler (first (:ops (abi/validate-batch
                                  (abi/encode-batch [[:dom/add-event-listener 1 :click "handler-1"]])))))))
  (is (= "handler-1"
         (:handler (first (:ops (abi/validate-batch
                                  (abi/encode-batch [[:dom/dispatch-event "handler-1" {:type :click}]])))))))
  (testing "nil is still rejected -- only nil, not every non-numeric value"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid add-event-listener"
                          (abi/validate-batch (abi/encode-batch [[:dom/add-event-listener 1 :click nil]]))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid dispatch-event"
                          (abi/validate-batch (abi/encode-batch [[:dom/dispatch-event nil {:type :click}]]))))))

(deftest abi-encodes-insert-before-and-remove-child-ops
  ;; Before this fix, kotoba.wasm.dom/insert-before and remove-child's own
  ;; emitted ops ([:dom/insert-before parent child before] /
  ;; [:dom/remove-child parent child], see kotoba.wasm.dom.cljc) had no
  ;; kotoba.wasm.abi/op->record case and fell through to its default branch,
  ;; throwing "Unknown kotoba DOM op". This proves both now encode to the
  ;; same {:op ... :parent ... :child ...}-shaped record convention
  ;; append-child/remove-children already use, and validate cleanly.
  (is (= {:abi/version 1
          :ops [{:op :insert-before :parent 1 :child 2 :before 3}]}
         (abi/encode-batch [[:dom/insert-before 1 2 3]])))
  (testing "insert-before's before id is optional (nil means append at the end, mirrors kotoba.wasm.dom/insert-before's own fallback)"
    (is (= {:abi/version 1
            :ops [{:op :insert-before :parent 1 :child 2 :before nil}]}
           (abi/encode-batch [[:dom/insert-before 1 2 nil]]))))
  (is (= {:abi/version 1
          :ops [{:op :remove-child :parent 1 :child 2}]}
         (abi/encode-batch [[:dom/remove-child 1 2]])))
  (is (= (abi/encode-batch [[:dom/insert-before 1 2 3]])
         (abi/validate-batch (abi/encode-batch [[:dom/insert-before 1 2 3]]))))
  (is (= (abi/encode-batch [[:dom/remove-child 1 2]])
         (abi/validate-batch (abi/encode-batch [[:dom/remove-child 1 2]])))))

(deftest dom-insert-before-and-remove-child-ops-replay-through-abi-and-retained-state
  ;; End-to-end: kotoba.wasm.dom's own document model -> the ops it emits ->
  ;; kotoba.wasm.abi/encode-batch (host/commit!'s exact encode path) ->
  ;; kotoba.wasm.host.retained/apply-op (the same reducer the webgl/webgpu
  ;; hosts use). All three layers must agree on the resulting child order.
  (let [[root document] (dom/create-element dom/empty-document :main)
        document (dom/set-root document root)
        [a document] (dom/create-element document :span)
        [b document] (dom/create-element document :span)
        document (-> document
                    (dom/append-child root a)
                    (dom/append-child root b))
        [c document] (dom/create-element document :span)
        document (dom/insert-before document root c b) ; [a b] -> [a c b]
        document (dom/remove-child document root a)    ; [a c b] -> [c b]
        [ops _document] (dom/consume-ops document)
        batch (abi/validate-batch (abi/encode-batch ops))
        retained-state (reduce retained/apply-op retained/base-state (:ops batch))]
    (testing "kotoba.wasm.dom's own document model reflects the edits"
      (is (= [c b] (get-in document [:nodes root :children]))))
    (testing "the ABI batch carries both ops through validate-batch without throwing"
      (is (some #(= :insert-before (:op %)) (:ops batch)))
      (is (some #(= :remove-child (:op %)) (:ops batch))))
    (testing "replaying the batch through the shared retained-state reducer agrees with the dom document"
      (is (= [c b] (get-in retained-state [:nodes root :children]))))))

(deftest host-commit-applies-insert-before-and-remove-child-ops
  ;; host/commit! (used by runtime/mount!/rerender!) calls abi/encode-batch
  ;; then applies every record to the host -- this is the exact call site
  ;; that used to crash with "Unknown kotoba DOM op" for these two ops.
  (let [dom-host (host/recording-host)]
    (host/commit! dom-host [[:dom/create-element 1 :main]
                           [:dom/set-root 1]
                           [:dom/create-element 2 :span]
                           [:dom/append-child 1 2]
                           [:dom/create-element 3 :span]
                           [:dom/insert-before 1 3 2]
                           [:dom/remove-child 1 2]])
    (is (= [:create-element :set-root :create-element :append-child
            :create-element :insert-before :remove-child]
           (mapv :op (:ops (host/recorded dom-host)))))))

(deftest dom-document-ops-cover-removal-dispatch-and-consume
  (let [[root-id document] (dom/create-element dom/empty-document :main)
        [child-id document] (dom/create-text-node document "x")
        document (-> document
                     (dom/set-root root-id)
                     (dom/append-child root-id child-id)
                     (dom/add-event-listener root-id :click 42))
        document (dom/dispatch-event document root-id :click {:x 1})
        [ops document] (dom/consume-ops document)
        document (dom/remove-children document root-id)]
    (is (= "x" (dom/text-content (assoc document :root root-id :nodes (assoc-in (:nodes document) [root-id :children] [child-id])))))
    (is (some #(= [:dom/dispatch-event 42 {:x 1}] %) ops))
    (is (= [[:dom/remove-children root-id]] (:ops document)))
    (is (= "" (dom/text-content document)))
    (is (= [[:dom/remove-children root-id]] (:ops document)))))

(deftest function-map-host-commits-through-same-runtime-path
  (let [state (atom {:ops [] :events [] :present-count 0})
        function-host {:apply-op! (fn [_ op] (swap! state update :ops conj op))
                       :present! (fn [_] (swap! state update :present-count inc))
                       :poll-event! (fn [_] nil)}
        batch (host/commit! function-host [[:dom/create-element 1 :button]
                                           [:dom/set-root 1]])]
    (is (= 1 (:present-count @state)))
    (is (= [:create-element :set-root] (mapv :op (:ops @state))))
    (is (= (:ops batch) (:ops @state)))))

(deftest host-commit-validates-before-mutating-host
  (let [state (atom {:ops [] :present-count 0})
        function-host {:apply-op! (fn [_ op] (swap! state update :ops conj op))
                       :present! (fn [_] (swap! state update :present-count inc))
                       :poll-event! (fn [_] nil)}]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid add-event-listener"
                          (host/commit! function-host [[:dom/create-element 1 :button]
                                                       [:dom/add-event-listener 1 nil 9]])))
    (is (= [] (:ops @state)))
    (is (zero? (:present-count @state)))))

(deftest runtime-event-normalization-and-unknown-handler-behavior
  (reset-counter! 0)
  (let [mounted (runtime/mount! (host/recording-host) rich-counter)
        handler-id (first (keys (:handlers mounted)))
        dom-host (host/recording-host [{:handler handler-id
                                        :target 4
                                        :name :click
                                        :x 10
                                        :y 20
                                        :value 9}
                                       {:handler 999 :target 4 :name :click}])
        mounted (runtime/mount! dom-host rich-counter)
        pumped (runtime/pump-events! mounted)]
    (is (= "Counterabinc1" (dom/text-content (:document pumped))))
    (is (= [{:target 4 :name "click" :x 10.0 :y 20.0 :value "9" :key nil}]
           @(rf/subscribe [:events])))))

(deftest runtime-pumps-input-and-key-events-from-reagent-listeners
  (reset-input-capture!)
  (let [mounted (runtime/mount! (host/recording-host) input-capture)
        input-id (->> (get-in mounted [:document :nodes])
                      (some (fn [[id node]]
                              (when (= "name" (get-in node [:attrs :id])) id))))
        ;; :listeners now holds an ORDERED collection of handler-ids per
        ;; (node, event-type), not a single scalar -- see kotoba.wasm.dom's
        ;; own multi-listener fix. Exactly one reagent-attached listener is
        ;; registered here, so `first` recovers the same real handler-id
        ;; this test always meant to grab.
        input-handler (first (get-in mounted [:document :listeners input-id :input]))
        key-handler (first (get-in mounted [:document :listeners input-id :key-down]))
        dom-host (host/recording-host [{:handler input-handler
                                        :target input-id
                                        :name :input
                                        :value "kotoba"}
                                       {:handler key-handler
                                        :target input-id
                                        :name :key-down
                                        :key :Enter}])
        mounted (runtime/mount! dom-host input-capture)
        pumped (runtime/pump-events! mounted)]
    (is (= "kotoba" (dom/text-content (:document pumped))))
    (is (= [{:target input-id :name "input" :x 0.0 :y 0.0 :value "kotoba" :key nil}
            {:target input-id :name "key-down" :x 0.0 :y 0.0 :value nil :key "Enter"}]
           @(rf/subscribe [:events])))))

(deftest layout-respects-basic-style-and-hit-semantics
  (reset-counter! 5)
  (let [tree (dom/tree (:document (r/render [rich-counter])))
        ops (layout/draw-ops tree {:width 480})
        button-rect (some #(when (and (= :rect (:draw/op %)) (= :button (:tag %))) %) ops)
        button-node (some #(when (and (= :node (:draw/op %)) (= :button (:tag %))) %) ops)]
    (is (= "#334455" (:color button-rect)))
    (is (= 120 (:w button-node)))
    (is (= "primary large hot wide" (:class button-node)))
    (is (= [:click] (:listeners button-node)))))
