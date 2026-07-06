(ns kotoba.wasm.dom-test
  (:require [clojure.test :refer [deftest is]]
            [kotoba.wasm.dom :as dom]))

(defn- dispatch-ops
  [document node-id event-name event]
  (let [document (dom/dispatch-event document node-id event-name event)
        [ops _] (dom/consume-ops document)]
    (filterv #(= :dom/dispatch-event (first %)) ops)))

(defn- fresh-button
  []
  (let [[node document] (dom/create-element dom/empty-document :button)
        document (dom/set-root document node)
        [_ document] (dom/consume-ops document)]
    [node document]))

(deftest two-listeners-on-the-same-node-and-event-both-fire
  ;; The confirmed repro: registering a second addEventListener on the same
  ;; (element, event-type) pair previously OVERWROTE the first -- only the
  ;; most-recently-added listener ever fired. Real HTML5 addEventListener
  ;; supports multiple independent listeners for the same pair (e.g. two
  ;; separate scripts each attaching their own click handler to the same
  ;; button), confirmed via direct REPL reproduction before this fix.
  (let [[node document] (fresh-button)
        document (-> document
                     (dom/add-event-listener node "click" "handler-A")
                     (dom/add-event-listener node "click" "handler-B"))
        [_ document] (dom/consume-ops document)]
    (is (= ["handler-A" "handler-B"]
           (mapv second (dispatch-ops document node "click" {:type "click"})))
        "both listeners fire, in registration order")))

(deftest removing-one-listener-leaves-the-other-untouched
  ;; The sibling bug: the pre-existing remove-event-listener (in
  ;; browser.dom-bridge) dissoc'd the WHOLE event-type entry regardless of
  ;; which handler-id was asked for, so removing ONE listener silently
  ;; wiped every listener for that event type on that node.
  (let [[node document] (fresh-button)
        document (-> document
                     (dom/add-event-listener node "click" "handler-A")
                     (dom/add-event-listener node "click" "handler-B")
                     (dom/remove-event-listener node "click" "handler-A"))
        [_ document] (dom/consume-ops document)]
    (is (= ["handler-B"]
           (mapv second (dispatch-ops document node "click" {:type "click"})))
        "only handler-A is gone -- handler-B still fires")))

(deftest removing-the-last-listener-is-a-genuine-no-op-on-dispatch
  (let [[node document] (fresh-button)
        document (-> document
                     (dom/add-event-listener node "click" "handler-A")
                     (dom/remove-event-listener node "click" "handler-A"))
        [_ document] (dom/consume-ops document)]
    (is (empty? (dispatch-ops document node "click" {:type "click"})))))

(deftest removing-the-last-listener-cleans-up-the-event-type-key
  (let [[node document] (fresh-button)
        document (-> document
                     (dom/add-event-listener node "click" "handler-A")
                     (dom/remove-event-listener node "click" "handler-A"))]
    (is (nil? (get-in document [:listeners node :click]))
        "no dangling empty collection left behind")))

(deftest registering-the-identical-handler-id-twice-is-idempotent
  (let [[node document] (fresh-button)
        document (-> document
                     (dom/add-event-listener node "click" "handler-A")
                     (dom/add-event-listener node "click" "handler-A"))
        [_ document] (dom/consume-ops document)]
    (is (= ["handler-A"]
           (mapv second (dispatch-ops document node "click" {:type "click"})))
        "a duplicate registration fires once, not twice")))

(deftest a-single-listener-still-fires-exactly-as-before
  (let [[node document] (fresh-button)
        document (dom/add-event-listener document node "click" "only-handler")
        [_ document] (dom/consume-ops document)]
    (is (= ["only-handler"]
           (mapv second (dispatch-ops document node "click" {:type "click"}))))))

(deftest different-event-types-on-the-same-node-stay-independent
  (let [[node document] (fresh-button)
        document (-> document
                     (dom/add-event-listener node "click" "click-handler")
                     (dom/add-event-listener node "mouseover" "hover-handler"))
        [_ document] (dom/consume-ops document)]
    (is (= ["click-handler"]
           (mapv second (dispatch-ops document node "click" {:type "click"})))
        "dispatching click never fires the mouseover listener")))

(deftest dispatch-with-no-listeners-registered-at-all-is-a-no-op
  (let [[node document] (fresh-button)]
    (is (empty? (dispatch-ops document node "click" {:type "click"})))))
