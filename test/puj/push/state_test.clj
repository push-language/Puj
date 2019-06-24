(ns puj.push.state-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [puj.push.state :as state]
            [puj.push.type :as typ]))


(deftest push-state-test
  (let [stack->type {:int (spec/get-spec ::typ/int)}
        empty-state (state/make-state stack->type)
        mock-program {:puj.push.program/code '(7 "Puj")}
        mock-state (-> (state/make-state typ/base-stack->type)
                       (state/set-stack :int '(5 3 1))
                       (state/set-stack :string '("a" "b")))]

    (testing "state spec validity"
      (is (spec/valid? ::state/state empty-state))
      (is (spec/valid? ::state/state mock-state)))

    (testing "state creation"
      (is (= empty-state
             {:inputs {}
              :stdout ""
              :untyped (state/queue)
              :stacks {:int '() :exec '()}})))

    (testing "flush stack"
      (is (= (state/flush-stack mock-state :int)
             {:inputs {}
              :stdout ""
              :untyped (state/queue)
              :stacks {:int '() :string '("a" "b") :exec '()}})))

    (testing "push item"
      (is (= (state/push-item mock-state :int 10)
             {:inputs {}
              :stdout ""
              :untyped (state/queue)
              :stacks {:int '(10 5 3 1) :string '("a" "b") :exec '()}})))

    (testing "pop item"
      (is (= (state/pop-item mock-state :int)
             {:inputs {}
              :stdout ""
              :untyped (state/queue)
              :stacks {:int '(3 1) :string '("a" "b") :exec '()}})))

    (testing "nth item"
      (is (= (state/nth-item mock-state :int 1) 3)))

    (testing "top item"
      (is (= (state/top-item mock-state :int) 5)))

    (testing "insert item"
      (is (= (state/insert-item mock-state :int 1 10)
             {:inputs {}
              :stdout ""
              :untyped (state/queue)
              :stacks {:int '(5 10 3 1) :string '("a" "b") :exec '()}})))

    (testing "set nth item"
      (is (= (state/set-nth-item mock-state :int 1 10)
             {:inputs {}
              :stdout ""
              :untyped (state/queue)
              :stacks {:int '(5 10 1) :string '("a" "b") :exec '()}})))

    (testing "loading a program"
      (is (= (state/load-program empty-state mock-program)
             {:inputs {}
              :stdout ""
              :untyped (state/queue)
              :stacks {:int '() :exec '(7 "Puj")}})))

    (testing "loading inputs"
      (is (= (state/load-inputs empty-state {:i 7 :s "Puj"})
             {:inputs {:i 7 :s "Puj"}
              :stdout ""
              :untyped (state/queue)
              :stacks {:int '() :exec '()}})))

    (testing "observing values from stacks"
      (is (= (state/observe-stacks mock-state [:int :string :int])
             [5 "a" 3])))

    (testing "pop collections of values from stacks"
      (is (= (state/pop-from-stacks mock-state [:int :string :int])
             {:inputs {}
               :stdout ""
               :untyped (state/queue)
               :stacks {:int '(1) :string '("b") :exec '()}})))

    (testing "push to stacks"
      (is (= (state/push-to-stacks mock-state ["z" 10 "y"] [:string :int :string])
             {:inputs {}
              :stdout ""
              :untyped (state/queue)
              :stacks {:int '(10 5 3 1) :string '("y" "z" "a" "b") :exec '()}})))

    (testing "state size"
      (is (= (state/size mock-state) 5)))

    (testing "pretty print"
      (state/pretty-print mock-state))))