(ns puj.push.type
  (:require [clojure.spec.alpha :as spec]
            [cuerdas.core :as s]))


(spec/def ::int int?)
(spec/def ::string string?)


(spec/def ::stack->type
  (spec/map-of keyword? spec/spec? :conform-keys true))


(def reserved-stack-names
  "A collection of Push stack names than cannot be associated with custom types."
  #{:exec :untyped :stdout :input})


(defn validate-stack->type
  "Raises error if argument is not a valid `stack->type`."
  [stack->type]
  (do
    (if (not (spec/valid? ::stack->type stack->type))
      (throw (AssertionError. (spec/explain-str ::stack->type stack->type))))
    (assert (not (some reserved-stack-names (keys stack->type)))
            "Given stack->type uses reserved stack names.")))


(def base-stack->type
  "A map of the basic, commonly used, Push stacks and their associated types."
  {:int (spec/get-spec ::int)
   :string (spec/get-spec ::string)})


(defn stack-for
  "Given a `stack->type`, return the name of the associated Push stack for the given value."
  [value stack->type]
  (let [matches (filter (fn [[ _ spc]] (spec/valid? spc value))
                        (vec stack->type))]
    (cond
      (empty? matches) nil
      (= (count matches) 1) (first (first matches))
      :else
      (throw (AssertionError.
               (s/format "Ambiguous Push type for value $v."
                         {:v value}))))))