(ns puj.push.instruction
  (:require [clojure.spec.alpha :as s]
            [cuerdas.core :as str]
            [puj.push.pushstate :as pushstate]
            [puj.util :as u]))

(s/def ::name keyword?)

(s/def ::code-blocks nat-int?)

(s/def ::logic-fn fn?)

(s/def ::stack-name keyword?)

(s/def ::stack-list (s/coll-of ::stack-name))

(s/def ::touched-stacks ::stack-list)

(s/def ::input-stacks ::stack-list)

(s/def ::output-stacks ::stack-list)

(s/def ::other-stacks ::stack-list)

(s/def ::output-stack ::stack-name)

(s/def ::instruction-meta
  (s/keys :req [::name ::code-blocks]))

(s/def ::instruction-base
  (s/and ::instruction-meta
         (s/keys :req [::logic-fn])))

(s/def ::simple-instruction
  (s/and ::instruction-base
         (s/keys :req [::input-stacks ::output-stacks])))

(s/def ::state-to-state-instruction
  (s/and ::instruction-base
         (s/keys :req [::touched-stacks])))

(s/def ::takes-state-instruction
  (s/and ::instruction-base
         (s/keys :req [::output-stacks ::other-stacks])))

(s/def ::emit-many-of-type-instruction
  (s/and ::instruction-base
         (s/keys :req [::input-stacks ::output-stack])))

(s/def ::instruction
  (s/or :simple ::simple-instruction
        :state-to-state ::state-to-state-instruction
        :takes-state ::takes-state-instruction
        :emit-many-of-type ::emit-many-of-type-instruction))


(defn simple-instruction
  "Creates an instruction which uses a standardized way of manipulating Push states.

  Simple instructions handle popping their own function arguments and pushing the return values.

  The first step of evaluating a simple instruction is to pop the arguments from the stacks corresponding
  to the instruction's `input-stacks`. If multiple occurrences of the same type are in `input-stacks`,
  items are taken from progressively deeper in that stack. If the stacks of the  Push state do not contain
  a sufficient number of items, the instruction does not modify the Push state.

  The popped arguments are then passed to the instruction's `logic` function to produce the return value(s).
  The return value(s) are then routed to the corresponding stacks specified in the instruction's `output-stacks`"
  [name input-stacks output-stacks code-blocks logic-fn]
  {:post [(s/valid? ::simple-instruction %)]}
  {::name          name
   ::input-stacks  (u/ensure-seq input-stacks)
   ::output-stacks (u/ensure-seq output-stacks)
   ::code-blocks   code-blocks
   ::logic-fn      logic-fn})


(defn eval-simple
  [instruction state]
  (let [input-stacks (::input-stacks instruction)
        args (pushstate/observe-stacks state input-stacks)]
    (if (some nil? args)                                    ; Check if there are enough arguments from required stacks
      state
      (let [results (apply (::logic-fn instruction) args)]
        (cond
          (= results :revert)
          state

          :else
          (-> state
              (pushstate/pop-from-stacks input-stacks)
              (pushstate/push-to-stacks
                ; push-to-stacks expects a collection. If results is not a collection, make it one.
                (u/ensure-seq results)
                (::output-stacks instruction))))))))


(defn state-to-state-instruction
  "Creates an instruction whose function take an entire Push state and returns entire Push state."
  [name touched-stacks code-blocks logic-fn]
  {:post [(s/valid? ::state-to-state-instruction %)]}
  {::name           name
   ::touched-stacks touched-stacks
   ::code-blocks    code-blocks
   ::logic-fn       logic-fn})


(defn eval-state-to-state
  [instruction state]
  (let [new-state ((::logic-fn instruction) state)]
    (if (= new-state :revert) state new-state)))


(defn takes-state-instruction
  "Creates an instruction that takes entire Push state and returns particular values.

  The logic function of a takes-state instruction accepts an entire PushState as input and produces either
  a `:revert` token or some returned values. The return values are routed to the corresponding stacks specified
  in the instruction's `output-stacks`.

  Additional Push types utilized by the instruction are denoted in `other-stacks`."
  [name output-stacks other-stacks code-blocks logic-fn]
  {:post [(s/valid? ::takes-state-instruction %)]}
  {::name          name
   ::output-stacks (u/ensure-seq output-stacks)
   ::other-stacks  other-stacks
   ::code-blocks   code-blocks
   ::logic-fn      logic-fn})


(defn eval-takes-state
  [instruction state]
  (let [results ((::logic-fn instruction) state)]
    (if (= results :revert)
      state
      (pushstate/push-to-stacks state results (::output-stacks instruction)))))


(defn emit-many-of-type-instruction
  "Creates an instruction that produces arbitrarily many values of a given PushType.

  `emit-many-of-type` instructions pop their arguments in the same way as simple instructions.

  Items are popped from the stacks corresponding the types denoted in the `input-stacks`. If multiple occurrences
  of the same type are in `input-stacks`, items are taken from progressively deeper in that stack. If the stacks
  of the Push state do not contain a sufficient number of items, the instruction does not modify the Push state.

  The popped arguments are then passed to the instruction's logic function to produce a collection of return values.
  All return values are pushed individually to the stack denoted in `output-stack`."
  [name input-stacks output-stack code-blocks logic-fn]
  {:post [(s/valid? ::emit-many-of-type-instruction %)]}
  {::name         name
   ::input-stacks (u/ensure-seq input-stacks)
   ::output-stack output-stack
   ::code-blocks  code-blocks
   ::logic-fn     logic-fn})


(defn eval-emit-many-of-type
  [instruction state]
  (let [input-stacks (::input-stacks instruction)
        args (reverse (pushstate/observe-stacks state input-stacks))]
    (if (some nil? args)
      state
      (let [results (apply (::logic-fn instruction) args)]
        (cond
          (= results :revert)
          state

          (not (coll? results))
          (throw (ex-info "Instruction result must be a collection."
                          {:found (str (type (results)))}))

          :else
          (-> state
              (pushstate/pop-from-stacks input-stacks)
              (pushstate/push-to-stacks results (repeat (count results) (::output-stack instruction)))))))))


(defn instruction-type
  [instruction]
  (first (s/conform ::instruction instruction)))


(def instruction-evalers
  {:simple            eval-simple
   :state-to-state    eval-state-to-state
   :takes-state       eval-takes-state
   :emit-many-of-type eval-emit-many-of-type})


(defn eval-instruction
  [instruction state]
  {:pre [(s/valid? ::instruction instruction)]}
  (let [kind (instruction-type instruction)]
    ((kind instruction-evalers) instruction state)))


(defn required-stacks
  [instruction]
  (let [kind (instruction-type instruction)]
    (set
      (cond
        (= :simple kind)
        (concat (u/ensure-seq (::input-stacks instruction))
                (u/ensure-seq (::output-stacks instruction)))

        (= :state-to-state kind)
        (::touched-stacks instruction)

        (= :takes-state kind)
        (concat (u/ensure-seq (::output-stacks instruction))
                (u/ensure-seq (::other-stacks instruction)))

        (= :emit-many-of-type kind)
        (conj (::input-stacks instruction) (::output-stack instruction))

        :else
        (s/assert ::instruction instruction)))))


(defn instruction-meta
  ([instruction]
   {:pre [(s/valid? ::instruction instruction)]}
   (select-keys instruction [::name ::code-blocks]))
  ([name code-blocks]
   {::name        name
    ::code-blocks code-blocks}))
