(ns puj.push.instruction-test
  (:require [clojure.test :refer :all]
            [puj.push.instruction :as i]
            [puj.push.instruction-set :as i-set]
            [puj.push.interpreter :as interp]
            [puj.push.pushstate :as st]
            [puj.push.type :as t]))


(deftest push-instruction-test
  (let [test-ctx (interp/push-context {:int (:int t/core-scalars)}
                                      {:simple            (i/simple-instruction :simple [:int :int] :int 0 #(list (+ %1 %2)))
                                       :state-to-state    (i/state-to-state-instruction :state-to-state [:int] 0 #(st/flush-stack % :int))
                                       :takes-state       (i/takes-state-instruction :takes-state :int [] 0 #(list (st/stack-size % :int)))
                                       :emit-many-of-type (i/emit-many-of-type-instruction :emit-many-of-type [:int :int] :int 0 (fn [x n] (repeat n x)))})
        empty-state (st/make-state (::t/type-library test-ctx))
        mock-state (st/set-stack empty-state :int '(5 3))
        get-meta #(i/instruction-meta (get-in test-ctx [::i-set/instruction-set %]))]

    (testing "simple instructions"
      (let [instruction-meta (get-meta :simple)]
        (is (= (interp/eval-push-unit empty-state instruction-meta test-ctx) empty-state))
        (is (= (interp/eval-push-unit mock-state instruction-meta test-ctx)
               {:inputs  {}
                :stdout  ""
                :untyped (st/queue)
                :stacks  {:int '(8) :exec '()}}))))

    (testing "state-to-state instructions"
      (let [instruction-meta (get-meta :state-to-state)]
        (is (= (interp/eval-push-unit empty-state instruction-meta test-ctx) empty-state))
        (is (= (interp/eval-push-unit mock-state instruction-meta test-ctx) empty-state))))

    (testing "takes-state instructions"
      (let [instruction-meta (get-meta :takes-state)]
        (is (= (interp/eval-push-unit empty-state instruction-meta test-ctx)
               {:inputs  {}
                :stdout  ""
                :untyped (st/queue)
                :stacks  {:int '(0) :exec '()}}))
        (is (= (interp/eval-push-unit mock-state instruction-meta test-ctx)
               {:inputs  {}
                :stdout  ""
                :untyped (st/queue)
                :stacks  {:int '(2 5 3) :exec '()}}))))

    (testing "emit-many-of-type instruction"
      (let [instruction-meta (get-meta :emit-many-of-type)]
        (is (= (interp/eval-push-unit empty-state instruction-meta test-ctx) empty-state))
        (is (= (interp/eval-push-unit mock-state instruction-meta test-ctx)
               {:inputs  {}
                :stdout  ""
                :untyped (st/queue)
                :stacks  {:int '(3 3 3 3 3) :exec '()}}))))))
