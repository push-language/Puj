(ns puj.push.unit-test
  (:require [clojure.test :refer :all]
            [puj.push.unit :as u]
            [puj.push.type :as typ]
            [puj.push.instruction :as i]))


(deftest push-literal-test
  (let [mock-type-lib (select-keys typ/core-scalars [:int :string])
        int-lit (u/literal 5 :int)]
    (testing "literal creation"
      (is (= (u/infer-literal 5 mock-type-lib) int-lit)))))


(deftest push-unit?-test
  (let [lit (u/literal 100 :int)]
    (testing "literals"
      (is (u/push-unit? lit))
      (is (not (u/push-unit? 100))))
    (testing "instruction meta"
      (is (u/push-unit? (i/instruction-meta :a 0))))
    (testing "code block"
      (is (u/push-unit? (list lit)))
      (is (not (u/push-unit? (list :foo)))))))
