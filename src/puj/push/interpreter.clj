(ns puj.push.interpreter
  (:require [clojure.set :as set]
            [puj.push.program :as prog]
            [puj.push.instruction :as instr]
            [puj.push.instruction-set :as i-set]
            [puj.push.pushstate :as pushstate]
            [puj.push.type :as t]
            [puj.push.unit :as u]
            [clojure.spec.alpha :as s]
            [puj.push.instruction :as i]))


(s/def ::push-context
  ;; @todo Add push constraints.
  (s/keys :req [::t/type-library ::i-set/instruction-set]))


(defn validate-push-context
  [ctx]
  (let [type-library (::t/type-library ctx)
        instruction-set (::i-set/instruction-set ctx)
        supported-stacks (t/supported-stacks type-library)
        iset-required-stacks (i-set/required-stacks instruction-set)]
    (if (not (set/subset? iset-required-stacks supported-stacks))
      (throw (ex-info "The Push context doesn't support all stacks required by the instruction set."
                      {:supported supported-stacks
                       :required  iset-required-stacks
                       :missing   (set/difference iset-required-stacks supported-stacks)})))
    true))


(defn push-context
  ([]
   (push-context (t/core-types) (i-set/base-instruction-set)))
  ([instruction-set]
   (push-context (t/core-types) instruction-set))
  ([type-library instruction-set]
   {:post [(validate-push-context %)]}
   {::t/type-library        type-library
    ::i-set/instruction-set instruction-set}))


(defn validate-program-context
  [ctx program]
  (let [type-library (::t/type-library ctx)
        supported-stacks (t/supported-stacks type-library)
        prog-required-stacks (set (doall (concat (vals (::prog/input-scheme program))
                                                 (vals (::prog/output-scheme program)))))]
    (if (not (set/subset? prog-required-stacks supported-stacks))
      (throw (ex-info "The Push context does not support the stacks required by the program."
                      {:supported supported-stacks
                       :required  prog-required-stacks
                       :missing   (set/difference prog-required-stacks supported-stacks)})))
    true))


(defn process-untyped
  "Route all items in the untyped queue onto their corresponding stacks by performing
  a lookup using the `type-library` mapping."
  [state type-library]
  (loop [new-state state]
    (if (empty? (::pushstate/untyped new-state))
      new-state
      (recur
        (let [val (first (::pushstate/untyped new-state))
              dest (t/infer-push-type val type-library)
              new-untyped (pop (::pushstate/untyped new-state))]
          (-> new-state
              (pushstate/push-item dest val)
              (assoc new-state ::pushstate/untyped new-untyped)))))))


(defn eval-push-unit
  [state unit {instruction-set ::i-set/instruction-set}]
  (let [[unit-type _] (s/conform ::u/unit unit)]
    (cond
      (= :instruction-meta unit-type)
      (let [instruction ((::instr/name unit) instruction-set)]
        (i/eval-instruction instruction state))

      (= :literal unit-type)
      (pushstate/push-item state (::t/type-name unit) (::u/value unit))

      (= :code-block unit-type)
      (pushstate/set-stack state
                           :exec
                           (concat unit (pushstate/get-stack state :exec)))

      :else
      (throw (ex-info "Found invalid unit during Push evaluation."
                      {:unit      unit
                       :unit-type unit-type})))))


(defn step
  "Perform one step of Push program evaluation."
  [state ctx]
  (let [next-unit (pushstate/top-item state :exec)]
    (-> (pushstate/pop-item state :exec)
        (eval-push-unit next-unit ctx))))


(defn run
  "Run a Push program."
  [program inputs ctx & {:keys [validate? print-trace?]}]
  {:pre [(s/valid? ::push-context ctx)]}
  (when validate?
    (validate-program-context program ctx))
  ; @TODO: Add some kind of flexible metering system.
  (let [type-lib (::t/type-library ctx)]
    (loop [state (-> (pushstate/make-state type-lib)
                     (pushstate/load-inputs inputs)
                     (pushstate/load-program program))]
      (when print-trace?
        (do
          (println)
          (pushstate/pretty-print state)))
      (if (empty? (pushstate/get-stack state :exec))
        (let [output-scheme-seq (seq (::prog/output-scheme program)) ; Create a stable ordering
              output-names (map first output-scheme-seq)
              output-stacks (map second output-scheme-seq)
              output-values (pushstate/observe-stacks state output-stacks)
              outputs (zipmap output-names output-values)]
          (prog/make-program-result program inputs outputs))
        (recur (-> (step state ctx)
                   (process-untyped type-lib)))))))
