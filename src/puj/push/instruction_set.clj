(ns puj.push.instruction-set
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [puj.push.instruction :as i]
            [puj.push.instructions.numeric :as numeric]
            [puj.push.instructions.text :as text]))


(s/def ::instruction-set
  (s/map-of ::i/name ::i/instruction))


(defn- base-instruction-generators []
  [numeric/instructions
   text/instructions])


(defn base-instruction-set
  "See `puj.push.core.base-instruction-set`."
  [& {:keys [name-regex related-stacks]}]
  (->> (base-instruction-generators)
       (mapcat (fn [gen]
                 (let [name-pred #(re-matches name-regex (name (::i/name %)))
                       stack-pred #(some (set related-stacks) (i/required-stacks %))
                       pred (cond
                              (and (nil? name-regex) (nil? related-stacks))
                              (fn [_] true)

                              (nil? name-regex)
                              stack-pred

                              (nil? stack-pred)
                              name-pred

                              :else
                              #(and (name-pred %) (stack-pred %)))]
                   (->> (gen)
                        (filter pred)
                        (map (fn [i] [(::i/name i) i]))))))
       (into {})))


(defn register
  "See `puj.push.core.instruction-set-register`."
  [instruction-set instruction]
  (assoc instruction-set (::i/name instruction) instruction))


(defn unregister
  "See `puj.push.core.instruction-set-unregister`."
  [instruction-set name]
  (dissoc instruction-set name))


(defn register-all
  "See `puj.push.core.instruction-set-register-all`."
  [instruction-set instructions]
  (s/valid? (s/coll-of ::i/instruction) instructions)
  (merge instruction-set
         (into {} (map (fn [i] [(::i/name i) i])))))


(defn required-stacks
  [instrution-set]
  (reduce set/union (map i/required-stacks (vals instrution-set))))
