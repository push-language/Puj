(ns puj.push.stack-test
  (:require [clojure.test :refer :all]
            [puj.push.stack :as stack]))


(deftest push-stack-test
  (let [mock-stack '(:a :b :c :d)]

    (testing "push item"
      (is (= (stack/push-item mock-stack :z) '(:z :a :b :c :d))))

    (testing "pop"
      (is (= (stack/pop-item mock-stack) '(:b :c :d)))
      (is (= (stack/pop-item mock-stack 2) '(:a :b :d)))
      (is (= (stack/pop-item '() 2) '())))

    (testing "nth"
      (is (= (stack/nth-item mock-stack 2) :c))
      (is (nil? (stack/nth-item '() 1))))

    (testing "insert"
      (is (= (stack/insert mock-stack 0 :z) '(:z :a :b :c :d)))
      (is (= (stack/insert mock-stack 2 :z) '(:a :b :z :c :d)))
      (is (= (stack/insert '() 0 :z) '(:z))))

    (testing "set-nth"
      (is (= (stack/set-nth mock-stack 0 :z) '(:z :b :c :d)))
      (is (= (stack/set-nth mock-stack 2 :z) '(:a :b :z :d)))
      (is (= (stack/set-nth '() 0 :z) '(:z))))))