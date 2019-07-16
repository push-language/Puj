(ns puj.util-test
  (:require [clojure.test :refer :all]
            [puj.util :refer :all]))


(deftest keyword-to-str-test
  (testing "make-collection function"
    (is (= "foo" (keyword-to-str :foo)))
    (is (= "puj.util-test/foo" (keyword-to-str ::foo)))))


(deftest distribution-test

  (testing "make distribution"
    (is (= (make-distribution) {}))
    (is (= (make-distribution :a 100) {:a 1}))
    (is (= (make-distribution :a 1 :b 2 :c 1) {:a 1/4 :b 1/2 :c 1/4}))
    (is (= (make-distribution :a 2.0 :b 0.0) {:a 1.0 :b 0.0})))


  (let [d (make-distribution "a" 1.0 "b" 10.0)]
    (testing "single sample distribution"
      (let [samples (map (fn [_] (sample-distribution d)) (range 1000))
            observed-distribution (group-by identity samples)
            a (count (get observed-distribution "a"))
            b (count (get observed-distribution "b"))]
        (is (> b a))
        (is (> a 0))))

    (testing "multiple sample distribution"
      (let [samples (sample-distribution d 1000)
            observed-distribution (group-by identity samples)
            a (count (get observed-distribution "a"))
            b (count (get observed-distribution "b"))]
        (is (> b a))
        (is (> a 0))))

    (testing "handles zero probability"
      (let [samples (sample-distribution {:a 0 :b 1} 1000)]
        (is (not (some #{:a} samples)))
        (is (not (some nil? samples)))))))