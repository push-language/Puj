(ns puj.push.interpreter-test
  (:require [clojure.test :refer :all]
            [puj.push.type :as t]
            [puj.push.interpreter :as interp]
            [puj.push.program :as prog]
            [puj.push.instruction-set :as i-set]
            [puj.push.unit :as u]))


(deftest push-interpreter-test
  (let [ctx (interp/push-context)
        instr-set (::i-set/instruction-set ctx)]

    (testing "simple program execution"
      (let [program (prog/make-program
                      (list (u/literal 1 :int) (u/literal 2 :int) (:int-add instr-set))
                      {}
                      {:i :int})]
        (is (= {::prog/program program
                ::prog/inputs {}
                ::prog/outputs {:i 3}}
               (interp/run program {} ctx :validate? true)))))

    (testing "nested code block execution"
      (let [program (prog/make-program
                      (list (u/literal 1 :int) (list (u/literal 2 :int) (:int-add instr-set)))
                      {}
                      {:i :int})]
        (is (= {::prog/program program
                ::prog/inputs {}
                ::prog/outputs {:i 3}}
               (interp/run program {} ctx :validate? true)))))
    ))
