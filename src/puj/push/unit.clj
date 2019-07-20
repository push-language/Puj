(ns puj.push.unit
  (:require [cuerdas.core :as str]
            [puj.push.pushstate :as pushstate]
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
    (pushstate/set-stack state :exec (concat this (pushstate/get-stack state :exec)))))


(defrecord Literal [value stack-name]
  PushUnit
  (push-unit-type [_] :literal)
  (eval-push-unit [_ state]
    (pushstate/push-item state stack-name value)))


(defn make-literal
  "Create a Push literal by recognizing the corresponding Push stack based on the `type-set`."
  [value type-set]
  (let [s (type/stack-for value type-set)]
    (if (not (nil? s))
      (Literal. value s))))


(defprotocol PushInstruction
  "An instruction of the Push language."
  (open-count [this] "The number of CodeBlocks to open following the instruction. Used by linear genomes.")
  (required-stacks [this] "A set of stack names relevant to the instruction."))

(defn make-collection
  "If thing is a collection, returns it. Otherwise, returns it in a vector"
  [thing]
  (if (coll? thing)
    thing
    [thing]))


(defrecord SimpleInstruction [input-stacks output-stacks opens func]
  PushInstruction
  (open-count [_] opens)
  (required-stacks [_] (set (concat input-stacks (make-collection output-stacks))))

  PushUnit
  (push-unit-type [_] :instruction)
  (eval-push-unit [this state]
    (let [args (reverse (pushstate/observe-stacks state input-stacks))]
      (if (some nil? args) ; Check if there are enough arguments from required stacks
        state
        (let [results (apply (:func this) args)]
          (cond
            (= results :revert)
            state

            :else
            (-> state
                (pushstate/pop-from-stacks input-stacks)
                (pushstate/push-to-stacks
                 ; push-to-stacks expects a collection. If results is not a collection, make it one.
                 (make-collection results)
                 ; same with output-stacks
                 (make-collection output-stacks)))))))))


(defn simple-instruction
  "Creates a SimpleInstruction which uses a standardized way of manipulating Push states. In other words, it
  handles popping its own function arguments and pushing the function return values.

  The first step of evaluating a SimpleInstruction is to pop the arguments from the stacks corresponding
  to the instruction's `input-stacks` which can either be a single stack name (keyword) or a vector of stack names.

  If multiple occurrences of the same type are in `input-stacks`, items are taken from progressively deeper
  in that stack. If the stacks of the  PushState do not contain a sufficient number of items, the instruction
  does not modify the PushState.

  The popped arguments are then passed to the instruction's function to produce the return value(s).
  The return value(s) are then routed to the corresponding stacks specified in the instruction's `output-stacks`
  which is also either a single stack name (keyword) or a vector of stack names."
  [input-stacks output-stacks opens func]
  (->SimpleInstruction input-stacks output-stacks opens func))


(defrecord StateToStateInstruction [used-stacks opens func]
  PushInstruction
  (open-count [_] opens)
  (required-stacks [_] (set used-stacks))

  PushUnit
  (push-unit-type [_] :instruction)
  (eval-push-unit [this state]
    (let [new-state ((:func this) state)]
      (if (= new-state :revert) state new-state))))


(defn state-to-state-instruction
  "Creates an instruction whose function take an entire Push state and returns entire Push state."
  [used-stacks opens func]
  (->StateToStateInstruction used-stacks opens func))


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
        (pushstate/push-to-stacks state results (make-collection output-stacks))))))


(defn takes-state-instruction
  "Creates a TakesStateInstruction that takes entire PushState and returns particular values. The function of a
  TakesStateInstruction accepts an entire PushState as input and produces either a `:revert` token
  or some return values.

  The return values are then routed to the corresponding stacks specified in the instruction's `output-stacks`.
  Additional PushTypes utilized by the instruction are denoted in `other-stacks`."
  [output-stacks other-stacks opens func]
  (->TakesStateInstruction output-stacks other-stacks opens func))


(defrecord ProducesManyOfTypeInstruction [input-stacks output-stack opens func]
  PushInstruction
  (open-count [_] opens)
  (required-stacks [_] (set (conj input-stacks output-stack)))

  PushUnit
  (push-unit-type [_] :instruction)
  (eval-push-unit [this state]
    (let [args (reverse (pushstate/observe-stacks state input-stacks))]
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
                (pushstate/pop-from-stacks input-stacks)
                (pushstate/push-to-stacks results (repeat (count results) output-stack)))))))))


(defn produces-many-of-type-instruction
  "Creates a `ProducesManyOfTypeInstruction` that produces arbitrarily many values of a given PushType.
  `ProducesManyOfTypeInstructions` pop their arguments in the same was as `SimpleInstructions`.

  Items are popped from the stacks corresponding the types denoted in the `input-stacks` which can either
  be a single stack name (keyword) or a vector of stack names. If multiple occurrences of the same type are
  in `input-stacks`, items are taken from progressively deeper in that stack. If the stacks of the PushState
  do not contain a sufficient number of items, the instruction does not modify the PushState.

  The popped arguments are then passed to the instruction's function to produce a collection of return values.
  All return values are pushed individually to the stack denoted in `output_stack`."
  [input-stacks output-stack opens func]
  (->ProducesManyOfTypeInstruction input-stacks output-stack opens func))
