(ns puj.push.state-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [puj.push.pushstate :as state]
            [puj.push.stack :as stack]
            [puj.push.type :as typ]
            [puj.push.config :as cfg]
            [puj.test-support :as ts]
            [puj.push.type :as t]))


(deftest push-state-test
  (let [type-library (select-keys t/core-scalars [:int :string])
        empty-state (state/make-state type-library (cfg/push-config))
        mock-program {:puj.push.program/code '(7 "Puj")}
        mock-state (-> empty-state
                       (state/set-stack :int '(5 3 1))
                       (state/set-stack :string '("a" "b")))]

    (testing "state spec validity"
      (is (spec/valid? ::state/state empty-state))
      (is (spec/valid? ::state/state mock-state)))

    (testing "state creation"
      (is (= {:inputs         {}
              :stdout         ""
              :untyped        (state/queue)
              :stacks         {:int '() :string '() :exec '()}
              ::stack/manager {::cfg/push-config  (cfg/push-config)
                               ::typ/type-library type-library}}
             empty-state)))

    (testing "flush stack"
      (is (= (ts/stacks->state {:int '() :string '("a" "b") :exec '()})
             (state/flush-stack mock-state :int))))

    (testing "push item"
      (is (= (ts/stacks->state {:int '(10 5 3 1) :string '("a" "b") :exec '()})
             (state/push-item mock-state :int 10))))

    (testing "pop item"
      (is (= (ts/stacks->state {:int '(3 1) :string '("a" "b") :exec '()})
             (state/pop-item mock-state :int))))

    (testing "nth item"
      (is (= (state/nth-item mock-state :int 1) 3)))

    (testing "top item"
      (is (= (state/top-item mock-state :int) 5)))

    (testing "insert item"
      (is (= (ts/stacks->state {:int '(5 10 3 1) :string '("a" "b") :exec '()})
             (state/insert-item mock-state :int 1 10))))

    (testing "set nth item"
      (is (= (ts/stacks->state {:int '(5 10 1) :string '("a" "b") :exec '()})
             (state/set-nth-item mock-state :int 1 10))))

    (testing "loading a program"
      (is (= (ts/stacks->state {:int '() :string '() :exec '(7 "Puj")})
             (state/load-program empty-state mock-program))))

    (testing "loading inputs"
      (is (= (assoc
               (ts/stacks->state {:int '() :string '() :exec '()})
               :inputs {:i 7 :s "Puj"})
             (state/load-inputs empty-state {:i 7 :s "Puj"}))))

    (testing "observing values from stacks"
      (is (= (state/observe-stacks mock-state [:int :string :int])
             [5 "a" 3])))

    (testing "pop collections of values from stacks"
      (is (= (ts/stacks->state {:int '(1) :string '("b") :exec '()})
             (state/pop-from-stacks mock-state [:int :string :int]))))

    (testing "push to stacks"
      (is (= (ts/stacks->state {:int '(10 5 3 1) :string '("y" "z" "a" "b") :exec '()})
             (state/push-to-stacks mock-state ["z" 10 "y"] [:string :int :string])))
      (is (= (ts/stacks->state {:int '(11 5 3 1) :string '("a" "b") :exec '()})
             (state/push-to-stacks mock-state [11] [:int])))
      (is (= (ts/stacks->state {:int '(5 3 1) :string '("puj" "a" "b") :exec '()})
             (state/push-to-stacks mock-state ["puj"] [:string]))))

    (testing "state size"
      (is (= (state/size mock-state) 5)))

    (testing "pretty print"
      (state/pretty-print mock-state))

    (testing "push manager limits"
      (is (= (ts/stacks->state {:int (list (:numeric-magnitude cfg/default-limits)) :string '()})
             (state/push-item empty-state :int 1e15)))
      (is (= (ts/stacks->state {:int (list (- (:numeric-magnitude cfg/default-limits))) :string '()})
             (state/push-item empty-state :int -1e15)))
      (is (= (ts/stacks->state {:int '() :string (list (apply str (repeat 1e4 "!")))})
             (state/push-item empty-state :string (apply str (repeat 1e5 "!"))))))))
