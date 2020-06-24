(ns puj.push.type
  (:require [clojure.spec.alpha :as spec]))

; Specs

(spec/def ::type-name keyword?)
(spec/def ::spec spec/spec?)
(spec/def ::coercer fn?)
(spec/def ::push-type (spec/keys :req [::type-name ::spec ::coercer]))
(spec/def ::type-library (spec/map-of ::type-name ::push-type))


; Scalars

(spec/def ::boolean boolean?)
(spec/def ::int int?)
(spec/def ::float float?)
(spec/def ::string string?)
(spec/def ::char char?)


(def core-scalars
  {:boolean {::type-name :boolean ::spec (spec/get-spec ::boolean) ::coercer boolean}
   :int     {::type-name :int ::spec (spec/get-spec ::int) ::coercer int}
   :float   {::type-name :float ::spec (spec/get-spec ::float) ::coercer float}
   :string  {::type-name :string ::spec (spec/get-spec ::string) ::coercer str}
   :char    {::type-name :char ::spec (spec/get-spec ::char) ::coercer char}})


; Vectors

(defn- spec->vec-spec
  [underlying]
  (spec/coll-of underlying :kind vector?))


(defn vector-type
  [element-type]
  (let [type-name (keyword (str (name (::type-name element-type)) "-vector"))]
    {::type-name    type-name
     ::spec         (spec->vec-spec (::spec element-type))
     ; @TODO: Heavy use of coercion and vector types will be slow.
     ::coercer      #(vec (map (::coercer element-type) %))
     ::element-type element-type}))


(defn make-vector-types
  [underlying-types]
  (map vector-type underlying-types))


; Misc.

(def reserved-stack-names
  "A collection of Push stack names than cannot be associated with custom types."
  #{:exec :untyped :stdout :input})


(defn core-types
  "Create the core Push types provided by Puj."
  [& {:keys [vectors?] :or {vectors? true}}]
  (let [core-vectors (when vector?
                       (->> core-scalars
                            vals
                            make-vector-types
                            (map (fn [t] [(::type-name t) t]))
                            (into {})))]
    (merge core-scalars core-vectors)))


(defn validate-type-library
  "Raises error if argument is not a valid `type-library`."
  [type-library]
  (do
    (when (not (spec/valid? ::type-library type-library))
      (throw (AssertionError. (spec/explain-str ::type-library type-library))))
    (assert (not (some reserved-stack-names
                       (keys type-library)))
            "Type library uses reserved stack names.")))


(defn supported-stacks
  [type-library]
  (set (keys type-library)))


; @TODO: Make this not break for empty vectors.
(defn infer-push-type
  "Given a `type-library`, return the name of the associated Push stack for the given value."
  [value type-library]
  (let [matched-types (->> type-library
                           (filter (fn [[_ push-type]]
                                     (spec/valid? (::spec push-type) value)))
                           (map second))]
    (cond
      (empty? matched-types)
      nil

      (= (count matched-types) 1)
      (first matched-types)

      :else
      (throw (ex-info "Ambiguous Push type for value."
                      {:value         value
                       :matched-types matched-types})))))


(defn valid?
  [push-type value]
  (spec/valid? (::spec push-type) value))


(defn coerce
  [push-type value]
  ((::coercer push-type) value))
