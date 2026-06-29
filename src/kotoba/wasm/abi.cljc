(ns kotoba.wasm.abi
  "Canonical ABI encoding for kotoba.wasm.dom ops."
  (:require [clojure.string :as str]))

(def version 1)

(defn- maybe-name [x]
  (some-> x name))

(defn- split-attr [k]
  (if-let [ns (when k (namespace k))]
    [ns (maybe-name k)]
    [nil (maybe-name k)]))

(defn op->record [op]
  (let [[kind & xs] op]
    (case kind
      :dom/create-element
      (let [[id tag] xs]
        {:op :create-element :id id :tag (maybe-name tag)})

      :dom/create-text
      (let [[id text] xs]
        {:op :create-text :id id :text (str text)})

      :dom/set-root
      (let [[id] xs]
        {:op :set-root :id id})

      :dom/set-attr
      (let [[id k v] xs
            [attr-ns attr-name] (split-attr k)]
        {:op :set-attr
         :id id
         :namespace attr-ns
         :name attr-name
         :value (str v)})

      :dom/append-child
      (let [[parent child] xs]
        {:op :append-child :parent parent :child child})

      :dom/remove-children
      (let [[id] xs]
        {:op :remove-children :id id})

      :dom/add-event-listener
      (let [[id event-name handler-id] xs]
        {:op :add-event-listener
         :id id
         :name (maybe-name event-name)
         :handler handler-id})

      :dom/dispatch-event
      (let [[handler event] xs]
        {:op :dispatch-event :handler handler :event event})

      (throw (ex-info "Unknown kotoba DOM op" {:op op})))))

(defn encode-batch [ops]
  {:abi/version version
   :ops (mapv op->record ops)})

(defn validate-batch [batch]
  (when-not (= version (:abi/version batch))
    (throw (ex-info "Unsupported kotoba DOM ABI version" {:batch batch :expected version})))
  (doseq [{:keys [op id parent child handler name tag text value]} (:ops batch)]
    (case op
      :create-element (when-not (and (int? id) (seq tag)) (throw (ex-info "Invalid create-element op" {:op op :id id :tag tag})))
      :create-text (when-not (and (int? id) (string? text)) (throw (ex-info "Invalid create-text op" {:op op :id id :text text})))
      :set-root (when-not (int? id) (throw (ex-info "Invalid set-root op" {:op op :id id})))
      :set-attr (when-not (and (int? id) (seq name) (string? value)) (throw (ex-info "Invalid set-attr op" {:op op :id id :name name :value value})))
      :append-child (when-not (and (int? parent) (int? child)) (throw (ex-info "Invalid append-child op" {:op op :parent parent :child child})))
      :remove-children (when-not (int? id) (throw (ex-info "Invalid remove-children op" {:op op :id id})))
      :add-event-listener (when-not (and (int? id) (seq name) (int? handler)) (throw (ex-info "Invalid add-event-listener op" {:op op :id id :name name :handler handler})))
      :dispatch-event (when-not (int? handler) (throw (ex-info "Invalid dispatch-event op" {:op op :handler handler})))
      (throw (ex-info "Invalid ABI op kind" {:op op}))))
  batch)

(defn host-event
  "Normalize host input into the WIT-level input-event record shape."
  [{:keys [target name x y value key] :as event}]
  {:target target
   :name (clojure.core/name name)
   :x (double (or x 0.0))
   :y (double (or y 0.0))
   :value (some-> value str)
   :key (cond
          (keyword? key) (clojure.core/name key)
          (some? key) (str key)
          :else nil)})

(defn event-handler-id [event]
  (or (:handler event) (:handler-id event)))

(defn css-style-attr? [op]
  (and (= :set-attr (:op op))
       (= "style" (:namespace op))))

(defn style-map [ops node-id]
  (into {}
        (for [{:keys [id namespace name value]} ops
              :when (and (= id node-id)
                         (= "style" namespace))]
          [(keyword (str/replace name #"_" "-")) value])))
