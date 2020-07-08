(ns puj.push.pushstate
  (:require [clojure.spec.alpha :as spec]
            [puj.push.stack :as stack]
            [puj.push.type :as typ]
            [puj.push.config :as cfg])
  (:import (clojure.lang PersistentQueue)))


(defn queue
  ([] (PersistentQueue/EMPTY))
  ([coll]
   (reduce conj PersistentQueue/EMPTY coll)))


(spec/def ::state
  (spec/keys :req [::stack/manager]
             :req-un [::inputs ::stdout ::stacks ::untyped]))


(defn make-state
  "Creates a Push state.

  The state will have one stack per Push type in the given `type-library`.

  The `push-config` is used to create a `stack-manager` that will enforce limits on the values
  being pushed onto the stacks.

  Push states hold:
  - a map of `:inputs` that can be referenced by input units.
  - a `:stdout` string that is appended to by printing instructions.
  - an `:untyped` queue that can accept instruction results with unknown types.
  - a stack manager that enforces limits on values pushed to stacks of certain types.
  "
  [type-library push-config]
  (typ/validate-type-library type-library)
  {:inputs         {}
   :stdout         ""  ; @TODO: Replace with generic IO stream.
   :stacks         (zipmap (conj (typ/supported-stacks type-library) :exec) (repeat (list)))
   :untyped        (queue)
   ::stack/manager {::typ/type-library type-library
                    ::cfg/push-config  push-config}})


(defn prepare-for-stack
  [type-name value stack-manager]
  (if (contains? typ/reserved-stack-names type-name)
    value
    (let [stack-type (type-name (::typ/type-library stack-manager))
          push-config (::cfg/push-config stack-manager)
          safe-value (cond-> value

                             (typ/num-type? stack-type)
                             (cfg/limit-number push-config)

                             (= (::typ/type-name stack-type) :string)
                             (cfg/limit-string push-config)

                             (typ/coll-type? stack-type)
                             (cfg/limit-collection push-config))]
      (if (typ/valid? stack-type safe-value)
        safe-value
        (typ/coerce stack-type safe-value)))))


(defn get-stack
  "Retrieves a state's stack by its name."
  [state stack-name]
  (get-in state [:stacks stack-name]))


(defn stack-size
  "Retrieves the size of a state's stack by its name."
  [state stack-name]
  (count (get-stack state stack-name)))


(defn set-stack
  "Sets a stack to be the new collection."
  [state stack-name coll]
  (assoc-in state [:stacks stack-name] (apply list (map #(prepare-for-stack stack-name % (::stack/manager state)) coll))))


(defn flush-stack
  "Sets a stack to be an empty stack."
  [state stack-name]
  (assoc-in state [:stacks stack-name] '()))


(defn push-item
  "Pushes a value to a stack."
  [state stack-name value]
  (let [coerced (prepare-for-stack stack-name value (::stack/manager state))
        stk (get-stack state stack-name)]
    (set-stack state stack-name (stack/push-item stk coerced))))


(defn pop-item
  "Pops an item off a stack.

  If no `ndx` is given, the top item is popped.
  If a `0 <= ndx < (count stack)` is provided, the element at the index is removed.
  Otherwise the state is unchanged."
  ([state stack-name]
   (pop-item state stack-name 0))
  ([state stack-name ndx]
   (let [stk (get-stack state stack-name)]
     (set-stack state stack-name (stack/pop-item stk ndx)))))


(defn nth-item
  "Retrieves the nth item of stack. If `ndx` is out of range, returns nil."
  [state stack-name ndx]
  (stack/nth-item (get-stack state stack-name) ndx))


(defn top-item
  "Retrieves the top item of stack. If the stack is empty, returns nil."
  [state stack-name]
  (stack/nth-item (get-stack state stack-name) 0))


(defn insert-item
  "Inserts a value at position `ndx` in a stack."
  [state stack-name ndx value]
  (let [coerced (prepare-for-stack stack-name value (::stack/manager state))
        stk (get-stack state stack-name)]
    (set-stack state stack-name (stack/insert stk ndx coerced))))


(defn set-nth-item
  "Overwrites a value at position `ndx` in a stack."
  [state stack-name ndx value]
  (let [coerced (prepare-for-stack stack-name value (::stack/manager state))
        stk (get-stack state stack-name)]
    (set-stack state stack-name (stack/set-nth stk ndx coerced))))


(defn load-program
  "Loads a Push program's code onto the Push state's `:exec` stack."
  [state program]
  (set-stack state :exec (concat (get-stack state :exec) (:puj.push.program/code program))))


(defn load-inputs
  "Sets the given map of inputs as the `inputs` of the Push state."
  [state inputs]
  (assoc state :inputs (into {} inputs)))


(defn observe-stacks
  "Return a list of values based on the given types indicated.

  Items are take from the tops of each stack. If multiple occurrences of the same stack are
  in `stack-names`, the returned values are taken from progressively deeper in that stack.
  Does not pop the values off the stacks."
  [state stack-names]
  (loop [remaining stack-names
         counts {}
         values []]
    (if (empty? remaining)
      values
      (let [next-stack (first remaining)
            ndx (get counts next-stack 0)
            next-value (nth-item state next-stack ndx)]
        (recur (rest remaining)
               (assoc counts next-stack (inc ndx))
               (conj values next-value))))))


(defn pop-from-stacks
  "Pop the top items for each `stack-name` and return the resulting state.

  If multiple occurrences of the same stack are in `stack-names`, progressively more items
  are popped from that stack."
  [state stack-names]
  (loop [remaining stack-names
         values []
         new-state state]
    (if (empty? remaining)
      new-state
      (let [next-stack-name (first remaining)
            next-stack (get-stack new-state next-stack-name)
            value (stack/nth-item next-stack 0)]
        (recur (rest remaining)
               (conj values value)
               (set-stack new-state next-stack-name (stack/pop-item next-stack)))))))


(defn push-to-stacks
  "Push all `values` them onto the correct stack denoted by `stack-names`."
  [state values stack-names]
  (let [pairs (map #(vector %1 %2) values stack-names)]
    (reduce (fn [state [value stack]]
              (push-item state stack value))
            state
            pairs)))


(defn size
  "Return the size of the Push state.

  Equal to the sum of all stack sizes and the size of the untyped queue."
  [state]
  (+ (apply + (map count (vals (:stacks state))))
     (count (:untyped state))))


(defn pretty-print
  [state]
  (doseq [stack-name (keys (:stacks state))]
    (printf "%s = " stack-name)
    (prn (get-stack state stack-name))
    (flush))
  (println "inputs =" (:inputs state))
  (println "stdout =" (:stdout state)))
