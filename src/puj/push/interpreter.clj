(ns puj.push.interpreter
  (:require [clojure.set :as set]
            [puj.push.program :as prog]
            [puj.push.instruction-set :as i-set]
            [puj.push.pushstate :as pushstate]
            [puj.push.type :as typ]
            [puj.push.unit :as u]
            [cuerdas.core :as str]))


(defn validate-interpretation-context
  "Raises an error if the given `program`, `type-set`, and `intruction-set` cannot be used
  together to evaluate Push programs."
  [program type-set instruction-set]
  (let [supported-stacks (typ/supported-stacks type-set)
        iset-required-stacks (i-set/required-stacks instruction-set)
        prog-required-stacks (set (doall (concat (vals (::prog/input-scheme program))
                                                 (vals (::prog/output-scheme program)))))]
    ; @TODO: provide better error messages.
    (assert (set/subset? iset-required-stacks supported-stacks)
            (str/format "$r not subset of $s" {:r (str iset-required-stacks) :s supported-stacks}))
    (assert (set/subset? prog-required-stacks supported-stacks))))


(defn process-untyped
  "Route all items in the untyped queue onto their corresponding stacks by performing
  a lookup using the `type-set` mapping."
  [state type-set]
  (loop [new-state state]
    (if (empty? (::pushstate/untyped new-state))
      new-state
      (recur
        (let [val (first (::pushstate/untyped new-state))
              dest (typ/stack-for val type-set)
              new-untyped (pop (::pushstate/untyped new-state))]
          (-> new-state
              (pushstate/push-item dest val)
              (assoc new-state ::pushstate/untyped new-untyped)))))))


(defn step
  "Perform one step of Push program evaluation."
  [state]
  (let [next-unit (pushstate/top-item state :exec)]
    (->> (pushstate/pop-item state :exec)
         (u/eval-push-unit next-unit))))


(defn run
  "Run a Push program."
  [program inputs type-set instruction-set & {:keys [validate?]}]
  (when validate?
    (validate-interpretation-context program type-set instruction-set))
  ; @TODO: Add some kind of flexible metering system.
  (loop [state (-> (pushstate/make-state type-set)
                   (pushstate/load-inputs inputs)
                   (pushstate/load-program program))]
    (if (empty? (pushstate/get-stack state :exec))
      (let [output-scheme-seq (seq (::prog/output-scheme program))  ; Create a stable ordering
            output-names (map first output-scheme-seq)
            output-stacks (map second output-scheme-seq)
            output-values (pushstate/observe-stacks state output-stacks)
            outputs (zipmap output-names output-values)]
        (prog/make-program-result program inputs outputs))
      (recur (process-untyped (step state) type-set)))))
