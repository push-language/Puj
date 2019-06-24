(ns puj.push.instruction-set
  (:require [clojure.spec.alpha :as spec]
            [clojure.set :as set]
            [puj.push.unit :as u]
            [puj.push.instructions.numeric :as numeric]
            [puj.push.instructions.text :as text]
            [puj.util :refer [keyword-to-str]]))


(spec/def ::push-instruction
  (partial extends? u/PushInstruction))

(spec/def ::instruction-set
  (spec/map-of keyword? ::push-instruction :conform-keys true))


(defn- base-instruction-generators []
  [numeric/instructions
   text/instructions])


(defn base-instruction-set
  [& {:keys [name-regex related-stack]}]
  ; @TODO: This implementation is slow and ugly. Replace later. DRY!
  (apply merge
    (map (fn [gen]
           (->> (gen)
                (filter (fn [[name instruction]]
                          (cond
                            (and (nil? name-regex) (nil? related-stack))
                            true

                            (nil? name-regex)
                            (some (set related-stack) (u/required-stacks instruction))

                            (nil? related-stack)
                            (re-matches name-regex (keyword-to-str name))

                            :else
                            (and (some (set related-stack) (u/required-stacks instruction))
                                 (re-matches name-regex (keyword-to-str name))))))
                (into {})))
         (base-instruction-generators))))


(defn register
  [instruction-set name instruction]
  (assoc instruction-set name instruction))


(defn unregister
  [instruction-set name]
  (dissoc instruction-set name))


(defn register-all
  [instruction-set instruction-map]
  (spec/valid? ::instruction-set instruction-map)
  (merge instruction-set instruction-map))


; @TODO: Add input instructions.
;(defn register-input-instructions
;  [instruction-set input-names])


(defn required-stacks
  [instrution-set]
  (reduce set/union (map #(u/required-stacks %) (vals instrution-set))))