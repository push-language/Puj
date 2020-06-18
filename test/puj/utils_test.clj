(ns puj.utils-test
  (:require [clojure.test :refer :all]
            [puj.util :as u]))


(deftest ensure-seq-test
  (is (= '(5) (u/ensure-seq 5)))
  (is (= [4] (u/ensure-seq [4])))
  (is (= '(:hello) (u/ensure-seq '(:hello)))))
