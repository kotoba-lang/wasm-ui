(ns kotoba.wasm.host.retained
  "Shared retained-tree host utilities for kotoba:dom renderers.
   WebGL, WebGPU, and native hosts should share this state transition layer so
   ABI semantics are tested once and renderer backends only handle drawing."
  (:require [cssom.layout :as layout]))

(def base-state
  {:nodes {}
   :listeners {}
   :events []
   :draw-ops []})

(defn- normalize-event-name [x]
  (keyword x))

(defn apply-op [state op]
  (case (:op op)
    :create-element
    (assoc-in state [:nodes (:id op)] {:node/id (:id op)
                                       :node/type :element
                                       :tag (keyword (:tag op))
                                       :attrs {}
                                       :children []})

    :create-text
    (assoc-in state [:nodes (:id op)] {:node/id (:id op)
                                       :node/type :text
                                       :text (:text op)})

    :set-root
    (assoc state :root (:id op))

    :set-attr
    (assoc-in state [:nodes (:id op) :attrs (if-let [ns (:namespace op)]
                                               (keyword ns (:name op))
                                               (keyword (:name op)))]
              (:value op))

    :append-child
    (update-in state [:nodes (:parent op) :children] (fnil conj []) (:child op))

    :remove-children
    (assoc-in state [:nodes (:id op) :children] [])

    :remove-child
    (update-in state [:nodes (:parent op) :children]
              (fn [children] (vec (remove #{(:child op)} (or children [])))))

    :insert-before
    (update-in state [:nodes (:parent op) :children]
              (fn [children]
                (let [children (vec (or children []))
                      before (:before op)]
                  (if (and before (some #{before} children))
                    (let [[pre post] (split-with #(not= before %) children)]
                      (vec (concat pre [(:child op)] post)))
                    (conj children (:child op))))))

    :add-event-listener
    (assoc-in state [:listeners (:id op) (normalize-event-name (:name op))] (:handler op))

    :remove-event-listener
    ;; Previously entirely unhandled here (falling through to the default
    ;; `state` below, a silent no-op) -- kotoba.wasm.abi's own encode/
    ;; validate-batch had no case for this op at all either (a real crash,
    ;; "Unknown kotoba DOM op", confirmed via direct REPL reproduction),
    ;; so no real removeEventListener() call had ever reached this far
    ;; before. Without this case, :listeners stayed stale after a real
    ;; removal -- a node's own node-tree projection (and this session's
    ;; own draw-ops :listeners field) would keep reporting a listener
    ;; that real dispatch logic elsewhere had already stopped calling.
    (update-in state [:listeners (:id op)] dissoc (normalize-event-name (:name op)))

    ;; Sibling gap to :remove-event-listener above: kotoba.wasm.abi had no
    ;; case for these four ops either (a real crash, fixed alongside this),
    ;; and this state layer never applied them -- confirmed via direct
    ;; REPL reproduction before touching source.
    :set-text
    (assoc-in state [:nodes (:id op) :text] (:text op))

    :create-fragment
    (assoc-in state [:nodes (:id op)] {:node/id (:id op)
                                       :node/type :document-fragment
                                       :children []})

    :focus
    (assoc state :focus (:id op))

    :blur
    ;; Real HTMLElement.blur() only clears focus when the blurring
    ;; element IS the currently-focused one -- blurring some OTHER,
    ;; not-currently-focused element must not steal focus away from a
    ;; genuinely different, already-focused element.
    (if (= (:focus state) (:id op)) (dissoc state :focus) state)

    state))

(defn node-tree [state id]
  (let [node (get-in state [:nodes id])]
    (case (:node/type node)
      :text (:text node)
      :element (assoc node
                      :listeners (keys (get-in state [:listeners id]))
                      :children (mapv #(node-tree state %) (:children node)))
      node)))

(defn draw-ops
  "Projects `state`'s retained node tree to a flat draw-ops vector (see
   cssom.layout/draw-ops). `measure-text` is an OPTIONAL real
   text-width function (`(fn [text font-size font-weight font-style]
   width-in-px)`) -- e.g. one
   backed by a real browser's `CanvasRenderingContext2D.measureText`,
   which the WebGL/WebGPU hosts already hold as `text-ctx` at the exact
   point they call this fn (see kotoba.wasm.host.webgl/webgpu's render!)
   -- passed straight through to cssom.layout/draw-ops' own optional
   `:measure-text` theme key, so this engine's word-wrap decisions can
   agree with how a real host will actually paint the already-wrapped
   text instead of cssom.layout's built-in per-character approximation.
   Omitting it (every caller before this arg existed, and every host
   without a real measurement function available) leaves cssom.layout's
   default char-w-approximation wrap behavior completely unaffected."
  ([state] (draw-ops state nil))
  ([state measure-text]
   (let [tree (when-let [root (:root state)]
                (node-tree state root))]
     (layout/draw-ops tree (cond-> {:width (:width state)}
                             measure-text (assoc :theme {:measure-text measure-text}))))))

(defn with-draw-ops
  ([state] (with-draw-ops state nil))
  ([state measure-text]
   (assoc state :draw-ops (draw-ops state measure-text))))

(defn enqueue-event [state event]
  (update state :events (fnil conj []) event))

(defn focus [state target]
  (assoc state :focus target))

(defn poll-event [state]
  (let [event (first (:events state))]
    [event (if event (update state :events subvec 1) state)]))

(defn hit-test [state x y event-name]
  (let [event-name (normalize-event-name event-name)]
    (->> (:draw-ops state)
         reverse
         (some (fn [op]
                 (when (and (= :node (:draw/op op))
                            (<= (:x op) x (+ (:x op) (:w op)))
                            (<= (:y op) y (+ (:y op) (:h op)))
                            (get-in state [:listeners (:id op) event-name]))
                   {:target (:id op)
                    :handler (get-in state [:listeners (:id op) event-name])}))))))

(defn listener-event
  ([state target event-name]
   (listener-event state target event-name nil))
  ([state target event-name extra]
   (let [event-name (normalize-event-name event-name)]
     (when-let [handler (get-in state [:listeners target event-name])]
       (merge {:handler handler
               :target target
               :name event-name}
              extra)))))

(defn hit-event
  ([state x y event-name]
   (hit-event state x y event-name nil))
  ([state x y event-name extra]
   (let [event-name (normalize-event-name event-name)]
     (when-let [{:keys [target]} (hit-test state x y event-name)]
       (listener-event state target event-name (merge {:x x :y y} extra))))))

(defn focused-event
  ([state event-name]
   (focused-event state event-name nil))
  ([state event-name extra]
   (when-let [target (:focus state)]
     (listener-event state target event-name extra))))

(defn queue-event [state event]
  (cond-> (enqueue-event state event)
    (:target event) (focus (:target event))))

(defn queue-hit-event
  ([state x y event-name]
   (queue-hit-event state x y event-name nil))
  ([state x y event-name extra]
   (if-let [event (hit-event state x y event-name extra)]
     (queue-event state event)
     state)))

(defn queue-focused-event
  ([state event-name]
   (queue-focused-event state event-name nil))
  ([state event-name extra]
   (if-let [event (focused-event state event-name extra)]
     (enqueue-event state event)
     state)))
