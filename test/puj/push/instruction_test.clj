(ns puj.push.instruction-test
  (:require [clojure.test :refer :all]))

;
;(deftest push-instruction-test
;  (let [mock-type-set #{{::typ/stack-name :int ::typ/spec (spec/get-spec ::typ/int) ::typ/coercer int}}
;        empty-state (st/make-state mock-type-set)
;        mock-state (st/set-stack empty-state :int '(5 3))]
;
;    (testing "simple instructions"
;      (let [push-int {::typ/stack-name :int ::typ/spec (spec/get-spec ::typ/int) ::typ/coercer int}
;            int-add (u/->SimpleInstruction [:int :int] [:int] 0 #(list (+ %1 %2)))]
;        (is (= (u/eval-push-unit int-add empty-state) empty-state))
;        (is (= (u/eval-push-unit int-add mock-state)
;               {:inputs {}
;                :stdout ""
;                :untyped (st/queue)
;                :stacks {:int '(8) :exec '()}}))))
;
;    (testing "state-to-state instructions"
;      (let [int-flush (u/->StateToStateInstruction [:int] 0 #(st/flush-stack % :int))]
;        (is (= (u/eval-push-unit int-flush empty-state) empty-state))
;        (is (= (u/eval-push-unit int-flush mock-state) empty-state))))
;
;    (testing "takes-state instructions"
;      (let [int-depth (u/->TakesStateInstruction [:int] [] 0 #(list (st/stack-size % :int)))]
;        (is (= (u/eval-push-unit int-depth empty-state)
;               {:inputs {}
;                :stdout ""
;                :untyped (st/queue)
;                :stacks {:int '(0) :exec '()}}))
;        (is (= (u/eval-push-unit int-depth mock-state)
;               {:inputs {}
;                :stdout ""
;                :untyped (st/queue)
;                :stacks {:int '(2 5 3) :exec '()}}))))
;
;    (testing "produces-many-of-type instruction"
;      (let [int-dup-n (u/->ProducesManyOfTypeInstruction [:int :int] :int 0 (fn [x n] (repeat n x)))]
;        (is (= (u/eval-push-unit int-dup-n empty-state) empty-state))
;        (is (= (u/eval-push-unit int-dup-n mock-state)
;               {:inputs {}
;                :stdout ""
;                :untyped (st/queue)
;                :stacks {:int '(3 3 3 3 3) :exec '()}}))))))
