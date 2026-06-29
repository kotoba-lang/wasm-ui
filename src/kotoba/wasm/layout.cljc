(ns kotoba.wasm.layout
  "Reference layout projection from kotoba virtual DOM to renderer draw ops.
   Real hosts can replace this with text shaping, flex/grid, WebGPU buffers, etc.
   The point is to keep a stable data boundary after DOM compatibility."
  (:require [clojure.string :as str]))

(def default-theme
  {:font-size 14
   :line-height 20
   :padding 4
   :gap 4
   :fg "#e6ebf5"
   :bg "#121724"
   :button-bg "#1f2738"})

(defn- parse-int [x fallback]
  (cond
    (integer? x) x
    (number? x) (int x)
    (string? x) (or #?(:clj (try (Integer/parseInt (re-find #"\d+" x))
                              (catch Exception _ nil))
                       :cljs (let [n (js/parseInt x 10)]
                               (when-not (js/isNaN n) n)))
                    fallback)
    :else fallback))

(defn- attr [node k]
  (get-in node [:attrs k]))

(defn- style [node k]
  (or (get-in node [:attrs (keyword "style" (name k))])
      (get-in node [:attrs :style k])))

(defn- listeners [node]
  (let [ls (:listeners node)]
    (cond
      (map? ls) (keys ls)
      (sequential? ls) ls
      (set? ls) (seq ls)
      :else nil)))

(defn- text-node? [node]
  (string? node))

(defn- text-size [theme text]
  {:w (+ (* (count text) (int (* 0.6 (:font-size theme)))) (* 2 (:padding theme)))
   :h (+ (:line-height theme) (* 2 (:padding theme)))})

(declare layout-node)

(defn- layout-children [theme x y width children]
  (loop [remaining children
         y y
         boxes []
         height 0]
    (if-let [child (first remaining)]
      (let [{:keys [box draw]} (layout-node theme x y width child)
            child-h (:h box)
            next-y (+ y child-h (:gap theme))]
        (recur (rest remaining)
               next-y
               (into boxes draw)
               (+ height child-h (:gap theme))))
      {:draw boxes :h (max 0 (- height (:gap theme)))})))

(defn layout-node
  ([node] (layout-node default-theme 0 0 320 node))
  ([theme x y width node]
   (cond
     (nil? node)
     {:box {:x x :y y :w 0 :h 0} :draw []}

     (text-node? node)
     (let [{:keys [w h]} (text-size theme node)]
       {:box {:x x :y y :w (min width w) :h h}
        :draw [{:draw/op :text :x (+ x (:padding theme)) :y (+ y (:padding theme))
                :text node :color (:fg theme) :font-size (:font-size theme)}]})

     (= :text (:node/type node))
     (layout-node theme x y width (:text node))

     (= :element (:node/type node))
     (let [tag (:tag node)
           pad (parse-int (style node :padding) (:padding theme))
           explicit-w (parse-int (style node :width) width)
           node-w (min width explicit-w)
           content-x (+ x pad)
           content-y (+ y pad)
           content-w (max 0 (- node-w (* 2 pad)))
           {:keys [draw h]} (layout-children theme content-x content-y content-w (:children node))
           node-h (parse-int (style node :height) (+ h (* 2 pad)))
           bg (case tag
                :button (or (style node :background) (:button-bg theme))
                :main nil
                :span nil
                (or (style node :background) (:bg theme)))
           rect (when bg {:draw/op :rect :x x :y y :w node-w :h node-h :color bg :tag tag})
           semantic {:draw/op :node :id (:node/id node) :tag tag :x x :y y :w node-w :h node-h
                     :class (attr node :class) :listeners (listeners node)}]
       {:box {:x x :y y :w node-w :h node-h}
        :draw (cond-> []
                rect (conj rect)
                true (conj semantic)
                true (into draw))})

     :else
     (layout-node theme x y width (str node)))))

(defn draw-ops
  ([tree] (draw-ops tree {}))
  ([tree opts]
   (:draw (layout-node (merge default-theme (:theme opts))
                       (or (:x opts) 0)
                       (or (:y opts) 0)
                       (or (:width opts) 320)
                       tree))))
