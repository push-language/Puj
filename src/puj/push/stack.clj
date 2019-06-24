(ns puj.push.stack)


(defn push-item
  "Pushes a value onto the stack. Alias for conj."
  [stack value]
  (conj stack value))


(defn pop-item
  "Pops an item off the stack.

  If no `ndx` is given, the top item is popped.
  If a `0 <= ndx < (count stack)` is provided, the element at the index is removed.
  Otherwise the stack is unchanged."
  ([stack]
    (pop stack))
  ([stack ndx]
   (if (or (<= (count stack) ndx)
           (neg? ndx))
     stack
     (keep-indexed #(if (not= %1 ndx) %2) stack))))


(defn nth-item
  "Retrieves the nth item of stack. If `ndx` is out of range, returns nil."
  [stack ndx]
  (nth stack ndx nil))


(defn insert
  "Inserts `value` at index `ndx` in the stack."
  [stack ndx value]
  (concat (take ndx stack)
          (conj (drop ndx stack) value)))


(defn set-nth
  "Overwrites the value at index `ndx` in the stack with the `value`."
  [stack ndx value]
  (concat (take ndx stack)
          (conj (drop (inc ndx) stack) value)))
