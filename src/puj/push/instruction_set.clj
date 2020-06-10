(ns puj.push.instruction-set
  (:require [clojure.spec.alpha :as spec]
            [clojure.set :as set]
            [puj.push.unit :as u]
            [puj.push.instructions.numeric :as numeric]
            [puj.push.instructions.text :as text]))


(spec/def ::push-instruction
  (partial extends? u/PushInstruction))

(spec/def ::instruction-set
  (spec/map-of keyword? ::push-instruction :conform-keys true))


(defn- base-instruction-generators []
  [numeric/instructions
   text/instructions])


(defn base-instruction-set
  "See `puj.push.core.base-instruction-set`."
  [& {:keys [name-regex related-stacks]}]
  ; @TODO: This implementation is slow and ugly. Replace later. DRY!
  (apply merge
    (map (fn [gen]
           (->> (gen)
                (filter (fn [[instr-name instruction]]
                          (cond
                            (and (nil? name-regex) (nil? related-stacks))
                            true

                            (nil? name-regex)
                            (some (set related-stacks) (u/required-stacks instruction))

                            (nil? related-stacks)
                            (re-matches name-regex (name instr-name))

                            :else
                            (and (some (set related-stacks) (u/required-stacks instruction))
                                 (re-matches name-regex (name instr-name))))))
                (into {})))
         (base-instruction-generators))))


(defn register
  "See `puj.push.core.instruction-set-register`."
  [instruction-set name instruction]
  (assoc instruction-set name instruction))


(defn unregister
  "See `puj.push.core.instruction-set-unregister`."
  [instruction-set name]
  (dissoc instruction-set name))


(defn register-all
  "See `puj.push.core.instruction-set-register-all`."
  [instruction-set instruction-map]
  (spec/valid? ::instruction-set instruction-map)
  (merge instruction-set instruction-map))


; @TODO: Add input instructions.
;(defn register-input-instructions
;  "See `puj.push.core.instruction-set-register-inputs`."
;  [instruction-set input-names])


(defn required-stacks
  [instrution-set]
  (reduce set/union (map #(u/required-stacks %) (vals instrution-set))))
