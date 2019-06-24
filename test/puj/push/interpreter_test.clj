(ns puj.push.interpreter-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [puj.push.interpreter :as interp]
            [puj.push.program :as prog]
            [puj.push.instruction-set :as i-set]
            [puj.push.unit :as u]
            [puj.push.type :as typ]))


(deftest push-interpreter-test
  (let [stack->type {:int (spec/get-spec ::typ/int)}
        i-set (i-set/base-instruction-set :name-regex #"int-add|int-sub")]

    (testing "simple program execution"
      (let [program (prog/make-program
                      (list (u/->Literal 1 :int) (u/->Literal 2 :int) (:int-add i-set))
                      {}
                      {:i :int})]
        (is (= (interp/run program {} stack->type i-set :validate? true)
               {::prog/program program
                ::prog/inputs {}
                ::prog/outputs {:i 3}}))))

    (testing "nested program execution"
      (let [program (prog/make-program
                      (list (u/->Literal 1 :int) (list (u/->Literal 2 :int) (:int-add i-set)))
                      {}
                      {:i :int})]
        (is (= (interp/run program {} stack->type i-set :validate? true)
               {::prog/program program
                ::prog/inputs {}
                ::prog/outputs {:i 3}}))))
    ))