(ns puj.push.unit
  (:require [clojure.spec.alpha :as s]
            [puj.push.type :as t]
            [puj.push.instruction :as i]))


(s/def ::value some?)

(s/def ::literal
  (s/keys :req [::value ::t/type-name]))

(s/def ::input-name string?)

(s/def ::input
  (s/keys :req [::input-name]))

(s/def ::code-block
  (s/coll-of ::unit :kind list?))

(s/def ::unit
  (s/or :literal ::literal
        :instruction-meta ::i/instruction-meta
        :code-block ::code-block))


(defn unit-type
  [unit]
  (first (s/conform ::unit unit)))


(defn push-unit?
  "Returns true if the given value extends the PushUnit protocol."
  [thing]
  (s/valid? ::unit thing))


(defn literal
  [value push-type-name]
  {:post [(s/valid? ::literal %)]}
  {::value value
   ::t/type-name push-type-name})


(defn infer-literal
  "Create a Push literal by recognizing the corresponding Push stack based on the `type-set`."
  [value type-set]
  (let [push-type (t/infer-push-type value type-set)]
    (if (not (nil? push-type))
      (literal value (::t/type-name push-type)))))
