(ns puj.push.stack-test
  (:require [clojure.test :refer :all]
            [puj.push.stack :as stack]))


(deftest push-stack-test
  (let [mock-stack '(:a :b :c :d)]

    (testing "push item"
      (is (= '(:z :a :b :c :d) (stack/push-item mock-stack :z))))

    (testing "pop"
      (is (= '(:b :c :d) (stack/pop-item mock-stack)))
      (is (= '(:a :b :d) (stack/pop-item mock-stack 2)))
      (is (= '() (stack/pop-item '() 2))))

    (testing "nth"
      (is (= :c (stack/nth-item mock-stack 2)))
      (is (nil? (stack/nth-item '() 1))))

    (testing "insert"
      (is (= '(:z :a :b :c :d) (stack/insert mock-stack 0 :z)))
      (is (= '(:a :b :z :c :d) (stack/insert mock-stack 2 :z)))
      (is (= '(:z) (stack/insert '() 0 :z))))

    (testing "set-nth"
      (is (= '(:z :b :c :d) (stack/set-nth mock-stack 0 :z)))
      (is (= '(:a :b :z :d) (stack/set-nth mock-stack 2 :z)))
      (is (= '(:z) (stack/set-nth '() 0 :z))))))
