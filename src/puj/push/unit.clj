(ns puj.push.unit
  (:require [cuerdas.core :as str]
            [puj.push.state :as state]
            [puj.push.type :as type])
  (:import (clojure.lang PersistentList)))


(defprotocol PushUnit
  "An element of a Push program."
  (push-unit-type [this] "A keyword denoting the kind of Push unit.")
  (eval-push-unit [this state] "Evaluate the item on the state and return the resulting state."))


(defn push-unit?
  "Returns true if the given value extends the PushUnit protocol."
  [thing]
  (extends? PushUnit thing))


(extend-protocol PushUnit
  PersistentList
  (push-unit-type [_] :code-block)
  (eval-push-unit [this state]
    ; @TODO: Should this check that all contents extend PushUnit?
    (state/set-stack state :exec (concat this (state/get-stack state :exec)))))


(defrecord Literal [value stack-name]
  PushUnit
  (push-unit-type [_] :literal)
  (eval-push-unit [_ state]
    (state/push-item state stack-name value)))


(defn make-literal
  "Create a Push literal by recognizing the corresponding Push stack based on the `stack->type`."
  [value stack->type]
  (let [s (type/stack-for value stack->type)]
    (if (not (nil? s))
      (Literal. value s))))


(defprotocol PushInstruction
  "An instruction of the Push language."
  (open-count [this] "The number of CodeBlocks to open following the instruction. Used by linear genomes.")
  (required-stacks [this] "A set of stack names relevant to the instruction."))


; 
(defrecord SimpleInstruction [input-stacks output-stacks opens func]
  PushInstruction
  (open-count [_] opens)
  (required-stacks [_] (set (concat input-stacks
                                    (if (coll? output-stacks) ; If output-stacks is not a collection, make it one.
                                      output-stacks
                                      [output-stacks]))))

  PushUnit
  (push-unit-type [_] :instruction)
  (eval-push-unit [this state]
    (let [args (state/observe-stacks state input-stacks)]
      (if (some nil? args) ; Check if there are enough arguments from required stacks
        state
        (let [results (apply (:func this) args)]
          (cond
            (= results :revert)
            state

            :else
            (-> state
                (state/pop-from-stacks input-stacks)
                (state/push-to-stacks
                 ; push-to-stacks expects a collection. If results is not a collection, make it one.
                 (if (coll? results)
                   results
                   [results])
                 ; same with output-stacks
                 (if (coll? output-stacks)
                   output-stacks
                   [output-stacks])))))))))


(defrecord StateToStateInstruction [used-stacks opens func]
  PushInstruction
  (open-count [_] opens)
  (required-stacks [_] (set used-stacks))

  PushUnit
  (push-unit-type [_] :instruction)
  (eval-push-unit [this state]
    (let [new-state ((:func this) state)]
      (if (= new-state :revert) state new-state))))


(defrecord TakesStateInstruction [output-stacks other-stacks opens func]
  PushInstruction
  (open-count [_] opens)
  (required-stacks [_] (set (concat output-stacks other-stacks)))

  PushUnit
  (push-unit-type [_] :instruction)
  (eval-push-unit [this state]
    (let [results ((:func this) state)]
      (if (= results :revert)
        state
        (state/push-to-stacks state results output-stacks)))))


(defrecord ProducesManyOfTypeInstruction [input-stacks output-stack opens func]
  PushInstruction
  (open-count [_] opens)
  (required-stacks [_] (set (conj input-stacks output-stack)))

  PushUnit
  (push-unit-type [_] :instruction)
  (eval-push-unit [this state]
    (let [args (state/observe-stacks state input-stacks)]
      (if (some nil? args)
        state
        (let [results (apply (:func this) args)]
          (cond
            (= results :revert)
            state

            (not (coll? results))
            (throw (AssertionError.
                     (str/format "Instruction result must be a collection. Got $t."
                                 {:t (str (type results))})))

            :else
            (-> state
                (state/pop-from-stacks input-stacks)
                (state/push-to-stacks results (repeat (count results) output-stack)))))))))
