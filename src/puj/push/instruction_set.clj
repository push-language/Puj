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
  "Returns a map containing instructions from the core set provided by Puj.

   If `name-regex` is provided, only the instructions with a matching name will be included
   in the output. If a set of `related-stacks` is provided, only the instructions which
   manipulate one or more of those stacks is included.
   "
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
  "Associate an `instruction` with a `name` in the `instruction-set`."
  [instruction-set instruction]
  (assoc instruction-set (::i/name instruction) instruction))


(defn unregister
  "Dissociate the instruction with the given `name` in `instruction-set`."
  [instruction-set name]
  (dissoc instruction-set name))


(defn register-all
  "Register a map of instructions (keys are names, values are instructions) into the `instruction-set`."
  [instruction-set instructions]
  {:pre [(s/valid? (s/coll-of ::i/instruction) instructions)]}
  (merge instruction-set
         (into {} (map (fn [i] [(::i/name i) i])))))


(defn required-stacks
  [instrution-set]
  (reduce set/union (map i/required-stacks (vals instrution-set))))
