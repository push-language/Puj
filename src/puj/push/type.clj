(ns puj.push.type
  (:require [clojure.spec.alpha :as s]))

; Specs

(s/def ::type-name keyword?)
(s/def ::spec s/spec?)
(s/def ::coercer fn?)
(s/def ::flag #{:collection :numeric})
(s/def ::flags (s/coll-of ::flag :kind set?))
(s/def ::push-type (s/keys :req [::type-name ::spec ::coercer]))
(s/def ::type-library (s/map-of ::type-name ::push-type))


; Scalars

(s/def ::boolean boolean?)
(s/def ::int int?)
(s/def ::float float?)
(s/def ::string string?)
(s/def ::char char?)


(defn make-push-type
  "Creates a Push type.

  The `name` of the type must be a keyword.

  The `spec` is a clojure spec for valid values of the type.

  The `coerce-fn` is used for canonicalization of values. For example, all numbers passed to the `float` stack
  are cast using the `(float _)` function.

  A set of `floats` can optionally be included in the Push type. Examples include `:numeric` and `:collection`.
  "
  ([name spec coerce-fn]
   (make-push-type name spec coerce-fn #{}))
  ([name spec coerce-fn flags]
   {::type-name name
    ::sepc spec
    ::coercer coerce-fn
    ::flags flags}))


(def core-scalars
  {:boolean {::type-name :boolean ::spec (s/get-spec ::boolean) ::coercer boolean}
   :int     {::type-name :int ::spec (s/get-spec ::int) ::coercer long ::flags #{:numeric}}
   :float   {::type-name :float ::spec (s/get-spec ::float) ::coercer float ::flags #{:numeric}}
   :string  {::type-name :string ::spec (s/get-spec ::string) ::coercer str}
   :char    {::type-name :char ::spec (s/get-spec ::char) ::coercer char}})


; Vectors

(defn- spec->vec-spec
  [underlying]
  (s/coll-of underlying :kind vector?))


(defn vector-type
  "Create a Push type representing a vector with elements of another Push type."
  [element-type]
  (let [type-name (keyword (str (name (::type-name element-type)) "-vector"))]
    {::type-name    type-name
     ::spec         (spec->vec-spec (::spec element-type))
     ; @TODO: Heavy use of coercion and vector types will be slow.
     ::coercer      #(vec (map (::coercer element-type) %))
     ::element-type element-type
     ::flags #{:collection}}))


(defn make-vector-types
  [underlying-types]
  (map vector-type underlying-types))


; Misc.

(def reserved-stack-names
  "A collection of Push stack names than cannot be associated with custom types."
  #{:exec :code :untyped :stdout :input})


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
    (when (not (s/valid? ::type-library type-library))
      (throw (AssertionError. (s/explain-str ::type-library type-library))))
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
                                     (s/valid? (::spec push-type) value)))
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
  "Checks if the `value` is a valid instance of the `push-type`."
  [push-type value]
  (s/valid? (::spec push-type) value))


(defn coerce
  "Attempts to coerce the `value` to conform to the `push-type`."
  [push-type value]
  ((::coercer push-type) value))


(defn coll-type?
  "Checks if the `push-type` is a collection type."
  [push-type]
  (contains? (::flags push-type) :collection))


(defn num-type?
  [push-type]
  "Checks if the `push-type` is a numeric type."
  (contains? (::flags push-type) :numeric))
