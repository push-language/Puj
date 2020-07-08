(ns puj.push.interpreter
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [puj.push.program :as prog]
            [puj.push.instruction :as instr]
            [puj.push.instruction-set :as i-set]
            [puj.push.pushstate :as pushstate]
            [puj.push.type :as t]
            [puj.push.unit :as u]
            [puj.push.instruction :as i]
            [puj.push.config :as cfg]))


(s/def ::push-context
  (s/keys :req [::t/type-library ::i-set/instruction-set]))


(defn validate-push-context
  "Validates the push context, `ctx`, ensuring all types required by the instruction set are in the type library."
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
  "Creates a push context.

  If not given an `instruction-set` or `type-library`, the core instructions and type library are used respectively.
  "
  ([]
   (push-context (i-set/base-instruction-set) (t/core-types)))
  ([instruction-set]
   (push-context instruction-set (t/core-types)))
  ([instruction-set type-library]
   {:post [(validate-push-context %)]}
   {::t/type-library        type-library
    ::i-set/instruction-set instruction-set}))


(defn validate-program-context
  "Validates that the push context, `ctx`, will be able to execute the given `program`.

  Checks that the type library supports the types required by the program signature.
  "
  [ctx program]
  (let [signature (::prog/signature program)
        type-library (::t/type-library ctx)
        supported-stacks (t/supported-stacks type-library)
        prog-required-stacks (set (doall (concat (vals (::prog/input-scheme signature))
                                                 (vals (::prog/output-scheme signature)))))]
    (if (not (set/subset? prog-required-stacks supported-stacks))
      (throw (ex-info "The Push context does not support the stacks required by the program."
                      {:supported supported-stacks
                       :required  prog-required-stacks
                       :missing   (set/difference prog-required-stacks supported-stacks)})))
    true))


(defn process-untyped
  "Routes all items in the untyped queue onto their corresponding stacks by performing
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
  "Evaluates the `unit` on the given push `state`.

  If `unit` is an `instruction-meta`, the corresponding instruction will be looked-up in the
  instruction set of the `push-context` and evaluate it.
  "
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
  "Perform one step of Push program evaluation and return a new push state.

  Guards against push states that grow faster than the growth limit from the push-config.
  "
  [state ctx config]
  (let [next-unit (pushstate/top-item state :exec)
        initial-size (pushstate/size state)
        popped (pushstate/pop-item state :exec)
        next-state (eval-push-unit popped next-unit ctx)]
    (if (> (- (pushstate/size next-state) initial-size)
           (get-in config [::cfg/limits :growth]))
      popped
      next-state)))


(defn run
  "Run a Push program.

  The `program` is a push program containing code and a signature. The `inputs` is a map
  of input values to pass to the program.

  The `ctx` is Push context containing the type library and instruction set.

  Optionally, extra validation can be enabled with `:validate? true`. This is typically disabled
  during evolution because the Puj is guaranteed to call the interpreter correctly.

  To print the current push-state after each step of program execution the `:print-trace? true`
  option can be supplied.

  The returned value will be a `program-result` containing the `program`, the `inputs`, and a map of the
  values output by the program corresponding to the types in the `program` signature.
  "
  [program inputs ctx & {:keys [validate? print-trace?]}]
  {:pre [(s/valid? ::push-context ctx)]}
  (when validate?
    (validate-program-context program ctx))
  (let [type-lib (::t/type-library ctx)
        config (get-in program [::prog/signature ::cfg/push-config])
        end-time (+ (System/currentTimeMillis) (get-in config [::cfg/limits :runtime]))
        step-limit (get-in config [::cfg/limits :step])]
    (loop [state (-> (pushstate/make-state type-lib config)
                     (pushstate/load-inputs inputs)
                     (pushstate/load-program program))
           n-steps 0]
      (when print-trace?
        (do
          (println)
          (println "Execution Step:" step)
          (pushstate/pretty-print state)))
      (if (or (empty? (pushstate/get-stack state :exec))
              (> n-steps step-limit)
              (> (System/currentTimeMillis) end-time))
        (let [output-scheme-seq (seq (get-in program [::prog/signature ::prog/output-scheme])) ; Create a stable ordering
              output-names (map first output-scheme-seq)
              output-stacks (map second output-scheme-seq)
              output-values (pushstate/observe-stacks state output-stacks)
              outputs (zipmap output-names output-values)]
          (prog/make-program-result program inputs outputs))
        (recur (-> (step state ctx config)
                   (process-untyped type-lib))
               (inc n-steps))))))
