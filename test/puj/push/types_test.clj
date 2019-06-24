(ns puj.push.types-test
  (:require [clojure.test :refer :all]
            [puj.push.type :as t]
            [clojure.spec.alpha :as spec]))


(deftest stack->type-test

  (testing "should be a map of keywords to specs"
    (is (spec/valid? ::t/stack->type t/base-stack->type)))

  (testing "accept a valid stack->type"
    (t/validate-stack->type t/base-stack->type))

  (testing "should recall a type by name"
    (is (= (:int t/base-stack->type) (spec/get-spec ::t/int))))

  (testing "can recall a corresponding type name"
    (is (= (t/stack-for 5 t/base-stack->type) :int))
    (is (nil? (t/stack-for 1.2 t/base-stack->type)))))