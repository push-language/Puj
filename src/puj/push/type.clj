(ns puj.push.type
  (:require [clojure.spec.alpha :as spec]
            [clojure.set :as set]
            [puj.util :as u]
            [cuerdas.core :as string]))

; Specs

(spec/def ::stack-name keyword?)
(spec/def ::spec spec/spec?)
(spec/def ::coercer fn?)
(spec/def ::push-type (spec/keys :req [::stack-name ::spec ::coercer]))
(spec/def ::type-set (spec/coll-of ::push-type :kind set?))


; Scalars

(spec/def ::boolean boolean?)
(spec/def ::int int?)
(spec/def ::float float?)
(spec/def ::string string?)
(spec/def ::char char?)


(def core-scalars
  #{{::stack-name :boolean ::spec (spec/get-spec ::boolean) ::coercer boolean}
    {::stack-name :int ::spec (spec/get-spec ::int) ::coercer int}
    {::stack-name :float ::spec (spec/get-spec ::float) ::coercer float}
    {::stack-name :string ::spec (spec/get-spec ::string) ::coercer str}
    {::stack-name :char ::spec (spec/get-spec ::char) ::coercer char}})


; Vectors

(defn- spec->vec-spec
  [underlying]
  (spec/coll-of underlying :kind vector?))


(defn make-vector-types
  [underlying-types]
  (->> underlying-types
       (map (fn [push-type]
              {::stack-name (keyword (str (u/keyword-to-str (::stack-name push-type)) "-vector"))
               ::spec (spec->vec-spec (::spec push-type))
               ; @TODO: Heavy use of coercion and vector types will be slow.
               ::coercer #(vec (map (::coercer push-type) %))}))
       (into #{})))


; Misc.

(def reserved-stack-names
  "A collection of Push stack names than cannot be associated with custom types."
  #{:exec :untyped :stdout :input})


(defn core-types
  "Create the core Push types provided by Puj."
  [& {:keys [vectors?] :or {vectors? true}}]
  (set/union core-scalars
             (when vectors? (make-vector-types core-scalars))))


(defn validate-type-set
  "Raises error if argument is not a valid `type-set`."
  [type-set]
  (do
    (u/ensure-valid (spec/get-spec ::type-set) type-set)
    (assert (not (some reserved-stack-names
                       (map ::stack-name type-set)))
            "Provided type-set uses reserved stack names.")))


(defn supported-stacks
  [type-set]
  (set (map ::stack-name type-set)))


; @TODO: Make this not break for empty vectors.
(defn stack-for
  "Given a `type-set`, return the name of the associated Push stack for the given value."
  [value type-set]
  (let [matched-types (filter (fn [push-type]
                                (spec/valid? (::spec push-type) value))
                              (seq type-set))
        stack-names (supported-stacks matched-types)]
    (cond
      (empty? stack-names) nil
      (= (count stack-names) 1) (first stack-names)
      :else
      (throw (AssertionError.
               (string/format "Ambiguous Push type for value $v."
                              {:v value}))))))

(defn valid?
  [push-type value]
  (spec/valid? (::spec push-type) value))


(defn coerce
  [push-type value]
  ((::coercer push-type) value))