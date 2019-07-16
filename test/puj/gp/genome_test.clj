(ns puj.gp.genome_test
  (:require [clojure.test :refer :all]
            [puj.gp.genome :refer :all]
            [puj.push.type :as t]
            [puj.push.instruction-set :as i-set]
            [puj.util :as u]
            [puj.push.unit :as unit]))


(deftest genome-to-code-test
  (let [instruction-set (i-set/base-instruction-set)]

    (testing "translate simple genome"
      (is (= (to-code [(unit/->Literal 1 :int)
                       (unit/->Literal 2 :int)
                       (:int-add instruction-set)])
             (list (unit/->Literal 1 :int)
                   (unit/->Literal 2 :int)
                   (:int-add instruction-set)))))

    (testing "translate genome to nested program"
      (is (= (to-code [(unit/->Literal 5 :int)
                       (:no-op-open instruction-set)
                       (unit/->Literal 4 :int)
                       (unit/->Literal 3 :int)
                       :close
                       (unit/->Literal 2 :int)])
             (list (unit/->Literal 5 :int)
                   (:no-op-open instruction-set)
                   (list (unit/->Literal 4 :int)
                         (unit/->Literal 3 :int))
                   (unit/->Literal 2 :int)))))

    (testing "translate w/extra closes"
      (is (= (to-code [:close (unit/->Literal 1 :int)
                       :close (unit/->Literal 2 :int)
                       :close (:int-add instruction-set) :close])
             (list (unit/->Literal 1 :int)
                   (unit/->Literal 2 :int)
                   (:int-add instruction-set)))))

    (testing "translate genome w/missing closes"
      (is (= (to-code [(unit/->Literal 5 :int)
                       (:no-op-open instruction-set)
                       (unit/->Literal 4 :int)
                       (unit/->Literal 3 :int)
                       (unit/->Literal 2 :int)])
             (list (unit/->Literal 5 :int)
                   (:no-op-open instruction-set)
                   (list (unit/->Literal 4 :int)
                         (unit/->Literal 3 :int)
                         (unit/->Literal 2 :int))))))))


(deftest geneome-generation-test
  (let [type-set (->> (t/core-types :vectors? false)
                      (filter (fn [t] (or (= (::t/stack-name t) :int)
                                          (= (::t/stack-name t) :string)))))
        instruction-set (i-set/base-instruction-set :related-stacks #{:int :string})
        literals [0 1 10 "a" "abc"]
        erc-generators [#(rand-int 10)]]

    (testing "generate genome - proportional - fixed size"
      (let [genome (generate 10 type-set instruction-set literals erc-generators)]
        (is (= (count genome) 10))
        (is (= (count (filter #(= % :close) genome)) 0))))

    (testing "generate genome - proportional - size range"
      (let [genome (generate [1 100] type-set instruction-set literals erc-generators)]
        (is (and (<= (count genome) 100)
                 (>= (count genome) 1)))
        (is (= (count (filter #(= % :close) genome)) 0))))

    (testing "generate genome - explicit distribution"
      (let [d (u/make-distribution :instruction 0
                                   :literal 1
                                   :close 0
                                   :erc 1)
            genome (generate 10 type-set instruction-set literals erc-generators :distribution d)]
        (is (= (count genome) 10))
        (is (every? #(= (unit/push-unit-type %) :literal) genome))))))
