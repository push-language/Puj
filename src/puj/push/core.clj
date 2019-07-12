(ns puj.push.core
  (:require [puj.push.type :as t]
            [puj.push.instruction-set :as i-set]
            [puj.push.interpreter :as interp]))


(defn core-types
  "Create the core Push types provided by Puj."
  [& {:keys [vectors?] :or {vectors? true}}]
  (t/core-types :vectors? vectors?))


(defn base-instruction-set
  "Returns a map containing instructions from the core set provided by Puj.

  If `name-regex` is provided, only the instructions with a matching name will
  be included in the output. If a set of `related-stacks` is provided, only the
  instructions which manipulate one or more of those stacks is included."
  [& {:keys [name-regex related-stacks]}]
  (i-set/base-instruction-set :name-regex name-regex :related-stacks related-stacks))


(defn instruction-set-register
  "Associate an `instruction` with a `name` in the `instruction-set`."
  [instruction-set name instruction]
  (i-set/register instruction-set name instruction))


(defn instruction-set-register-all
  "Register a map of instructions (keys are names, values are instructions) into the `instruction-set`."
  [instruction-set instruction-map]
  (i-set/register-all instruction-set instruction-map))


;@TODO: (defn instruction-set-register-inputs
;  "Register a set of input instructions based on the names of a program's input values."
;  [instruction-set input-names])


(defn instruction-set-unregister
  "Dissociate the instruction with the given `name` in `instruction-set`."
  [instruction-set name]
  (i-set/unregister instruction-set name))


(defn run-push
  "Run a Push program."
  [program inputs type-set instruction-set & {:keys [validate?]}]
  (interp/run program inputs type-set instruction-set :validate? validate?))