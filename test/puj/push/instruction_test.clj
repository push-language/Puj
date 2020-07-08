(ns puj.push.instruction-test
  (:require [clojure.test :refer :all]
            [puj.push.instruction :as i]
            [puj.push.instruction-set :as i-set]
            [puj.push.interpreter :as interp]
            [puj.push.pushstate :as st]
            [puj.push.type :as t]
            [puj.push.config :as cfg]
            [puj.test-support :as ts]))


(deftest push-instruction-test
  ;; Here we will test the evaluation of various instruction types.

  (let [;; We start by creating our push-context that defines the stack types and instruction set.
        ;; We only need a small number of instructions per instruction type because all instructions of the same kind
        ;; will follow the same code paths.
        test-ctx (interp/push-context {:add               (i/simple-instruction :add [:int :int] :int 0 +)
                                       :one-hundred       (i/simple-instruction :one-hundred [] :int 0 (fn [] 100))
                                       :state-to-state    (i/state-to-state-instruction :state-to-state [:int] 0 #(st/flush-stack % :int))
                                       :stack-size        (i/takes-state-instruction :stack-size :int [] 0 #(st/stack-size % :int))
                                       :emit-many-of-type (i/emit-many-of-type-instruction :emit-many-of-type [:int :int] :int 0 (fn [x n] (repeat n x)))}
                                      {:int (:int t/core-scalars)})
        ;; An empty state with the correct stacks.
        empty-state (st/make-state (::t/type-library test-ctx) (cfg/push-config))
        ;; A simple state with some initial values.
        mock-state (st/set-stack empty-state :int '(5 3 0))
        ;; A helper function to pull an instruction-meta from an instruction.
        get-meta #(i/instruction-meta (get-in test-ctx [::i-set/instruction-set %]))]

    (testing "simple instructions"
      (let [;; Takes 2 args
            add (get-meta :add)
            ;; Takes no args
            one-hundred (get-meta :one-hundred)]
        ;; The add instruction should no-op on emtpy states/stacks and return an empty state.
        (is (= (interp/eval-push-unit empty-state add test-ctx) empty-state))
        ;; The one-hundred instruction requires no args and should push 100.
        (is (= (interp/eval-push-unit empty-state one-hundred test-ctx)
               (ts/stacks->state {:int '(100) :exec '()})))
        ;; A standard call to the add instruction.
        (is (= (interp/eval-push-unit mock-state add test-ctx)
               (ts/stacks->state {:int '(8 0) :exec '()})))))

    (testing "state-to-state instructions"
      ;; There isn't much to generically test for state-to-state instructions.
      ;; Each one should be tested individually in core_instructions_tests.clj
      (let [instruction-meta (get-meta :state-to-state)]
        (is (= (interp/eval-push-unit empty-state instruction-meta test-ctx) empty-state))
        (is (= (interp/eval-push-unit mock-state instruction-meta test-ctx) empty-state))))

    (testing "takes-state instructions"
      ;; There isn't much to generically test for takes-state instructions.
      ;; Each one should be tested individually in core_instructions_tests.clj
      (let [stack-size (get-meta :stack-size)]
        (is (= (interp/eval-push-unit empty-state stack-size test-ctx)
               (ts/stacks->state {:int '(0) :exec '()})))
        (is (= (interp/eval-push-unit mock-state stack-size test-ctx)
               (ts/stacks->state {:int '(3 5 3 0) :exec '()})))))

    (testing "emit-many-of-type instruction"
      (let [instruction-meta (get-meta :emit-many-of-type)]
        ;; Emit-many-of-type instructions pop arguments in the same way as simple-instructions.
        ;; They should no-op on emtpy states/stacks and return an empty state.
        (is (= (interp/eval-push-unit empty-state instruction-meta test-ctx) empty-state))
        ;; A standard call.
        (is (= (interp/eval-push-unit mock-state instruction-meta test-ctx)
               (ts/stacks->state {:int '(3 3 3 3 3 0) :exec '()})))))))
