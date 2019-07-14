(ns puj.push.unit-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [puj.push.unit :as u]
            [puj.push.pushstate :as st]
            [puj.push.type :as typ]
            [puj.push.pushstate :as state]))


(deftest make-collection-test
  (testing "make-collection function"
    (is (= [5] (u/make-collection 5)))
    (is (= [4] (u/make-collection [4])))
    (is (= '(:hello) (u/make-collection '(:hello))))))


(deftest push-literal-test
  (let [int-lit (u/->Literal 5 :int)
        str-lit (u/->Literal "Foo" :string)
        mock-type-set #{{::typ/stack-name :int ::typ/spec (spec/get-spec ::typ/int) ::typ/coercer int}
                        {::typ/stack-name :string ::typ/spec (spec/get-spec ::typ/string) ::typ/coercer str}}
        empty-state (st/make-state mock-type-set)]

    (testing "literal creation"
      (is (= (u/make-literal 5 mock-type-set) int-lit)))

    (testing "literal evaluation"
      (is (= (u/eval-push-unit int-lit empty-state)
             {:inputs {}
              :stdout ""
              :untyped (st/queue)
              :stacks {:int '(5) :string '() :exec '()}}))
      (is (= (->> empty-state (u/eval-push-unit str-lit) (u/eval-push-unit int-lit))
             {:inputs {}
              :stdout ""
              :untyped (st/queue)
              :stacks {:int '(5) :string '("Foo") :exec '()}})))))


(deftest push-instruction-test
  (let [mock-type-set #{{::typ/stack-name :int ::typ/spec (spec/get-spec ::typ/int) ::typ/coercer int}}
        empty-state (st/make-state mock-type-set)
        mock-state (st/set-stack empty-state :int '(5 3))]

    (testing "simple instructions"
      (let [int-add (u/->SimpleInstruction [:int :int] [:int] 0 #(list (+ %1 %2)))]
        (is (= (u/eval-push-unit int-add empty-state) empty-state))
        (is (= (u/eval-push-unit int-add mock-state)
               {:inputs {}
                :stdout ""
                :untyped (st/queue)
                :stacks {:int '(8) :exec '()}}))))

    (testing "state-to-state instructions"
      (let [int-flush (u/->StateToStateInstruction [:int] 0 #(state/flush-stack % :int))]
        (is (= (u/eval-push-unit int-flush empty-state) empty-state))
        (is (= (u/eval-push-unit int-flush mock-state) empty-state))))

    (testing "takes-state instructions"
      (let [int-depth (u/->TakesStateInstruction [:int] [] 0 #(list (state/stack-size % :int)))]
        (is (= (u/eval-push-unit int-depth empty-state)
               {:inputs {}
                :stdout ""
                :untyped (st/queue)
                :stacks {:int '(0) :exec '()}}))
        (is (= (u/eval-push-unit int-depth mock-state)
               {:inputs {}
                :stdout ""
                :untyped (st/queue)
                :stacks {:int '(2 5 3) :exec '()}}))))

    (testing "produces-many-of-type instruction"
      (let [int-dup-n (u/->ProducesManyOfTypeInstruction [:int :int] :int 0 (fn [x n] (repeat n x)))]
        (is (= (u/eval-push-unit int-dup-n empty-state) empty-state))
        (is (= (u/eval-push-unit int-dup-n mock-state)
               {:inputs {}
                :stdout ""
                :untyped (st/queue)
                :stacks {:int '(5 5 5) :exec '()}}))))))


(deftest push-code-block-test
  (let [mock-type-set #{{::typ/stack-name :int ::typ/spec (spec/get-spec ::typ/int) ::typ/coercer int}}
        empty-state (st/make-state mock-type-set)
        lit (u/->Literal 5 :int)
        instr (u/->SimpleInstruction [:int] [:int] 0 #(list (inc %)))
        code-block (list lit instr)]

    (testing "code-block unpack"
      (is (= (u/eval-push-unit code-block empty-state)
             {:inputs {}
              :stdout ""
              :untyped (st/queue)
              :stacks {:int '() :exec (list lit instr)}})))))
