(ns puj.push.state
  (:require [clojure.spec.alpha :as spec]
            [puj.push.stack :as stack]
            [puj.push.type :as typ])
  (:import (clojure.lang PersistentQueue)))


(defn queue
  ([] (PersistentQueue/EMPTY))
  ([coll]
   (reduce conj PersistentQueue/EMPTY coll)))


(spec/def ::state
  (spec/keys :req-un [::inputs ::stdout ::stacks ::untyped]))


(defn make-state
  "Creates a Push state with one stack per Push type in the given `type-set`."
  [type-set]
  (typ/validate-type-set type-set)
  {:inputs {}
   :stdout ""  ; @TODO: Replace with generic IO stream.
   :stacks (zipmap (conj (typ/supported-stacks type-set) :exec) (repeat (list)))
   :untyped (queue)})


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
  (assoc-in state [:stacks stack-name] coll))


(defn flush-stack
  "Sets a stack to be an empty stack."
  [state stack-name]
  (assoc-in state [:stacks stack-name] '()))


(defn push-item
  "Pushes a value to a stack."
  [state stack-name value]
  (let [stk (get-stack state stack-name)]
    (set-stack state stack-name (stack/push-item stk value))))


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
  (let [stk (get-stack state stack-name)]
    (set-stack state stack-name (stack/insert stk ndx value))))


(defn set-nth-item
  "Overwrites a value at position `ndx` in a stack."
  [state stack-name ndx value]
  (let [stk (get-stack state stack-name)]
    (set-stack state stack-name (stack/set-nth stk ndx value))))


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

  Equal to the sum of all stack sizes and they size of the untyped queue."
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