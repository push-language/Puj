(ns puj.push.types-test
  (:require [clojure.test :refer :all]
            [puj.push.type :as typ]
            [clojure.spec.alpha :as spec]
            [puj.push.type :as type]))


(deftest push-type-test

  (let [int-type {::typ/stack-name :int ::typ/spec (spec/get-spec ::typ/int) ::typ/coercer int}
        bool-type {::typ/stack-name :boolean ::typ/spec (spec/get-spec ::typ/boolean) ::typ/coercer boolean}]

    (testing "check scalar type"
      (is (typ/valid? int-type 5))
      (is (not (typ/valid? int-type true)))
      (is (not (typ/valid? int-type [5])))
      (is (typ/valid? bool-type true))
      (is (not (typ/valid? bool-type 5))))

    (testing "scalar coercion"
      (is (= (typ/coerce int-type 1.1) 1))
      (is (typ/coerce bool-type 50))
      (is (not (typ/coerce bool-type nil))))

    (let [int-vec-type (first (typ/make-vector-types #{int-type}))]
      (testing "check vector type"
        (is (typ/valid? int-vec-type [3 2 1]))
        (is (not (typ/valid? int-vec-type [5 "a"])))
        (is (not (typ/valid? int-vec-type 10))))

      (testing "coerce a vector"
        (is (= (type/coerce int-vec-type '(1 2 3)) [1 2 3]))
        (is (= (type/coerce int-vec-type [1 2.5 3]) [1 2 3]))
        (is (= (type/coerce int-vec-type '(1.2 3.4)) [1 3]))))))


(deftest type-set-test

  (let [type-set (typ/core-types)]

    (testing "can validate a type-set"
      (typ/validate-type-set type-set))

    (testing "prevent reserved stacks from being used"
      (is (thrown? AssertionError
                   (typ/validate-type-set #{::type/stack-name :exec ::type/spec any? ::type/coercer identity}))))

    (testing "can produce the corresponding stack name for a value"
      (is (= (typ/stack-for 5 type-set) :int))
      (is (= (typ/stack-for 1.2 type-set) :float))
      (is (= (typ/stack-for ["hi" "bye"] type-set) :string-vector)))))