(ns puj.push.core-instructions-test
  (:require [clojure.test :refer :all]
            [puj.push.interpreter :as interp]
            [puj.push.instruction-set :as i-set]
            [puj.push.pushstate :as st]))


(deftest core-instructions
  (let [ctx (interp/push-context)
        stacks->state #(assoc (st/make-state {}) :stacks %)]
    (are
      [instr-name initial expected]
      (= (stacks->state expected)
         (interp/eval-push-unit (stacks->state initial) (get-in ctx [::i-set/instruction-set instr-name]) ctx))

      :int-add       ;; Name of the instruction being tested.
      {:int '(2 1)}  ;; Initial state of stacks.
      {:int '(3)}    ;; Expected resulting state of stacks.

      :int-sub
      {:int '(3 1)}
      {:int '(2)}

      :int-sub
      {:int '(1 10)}
      {:int '(-9)}

      :int-mult
      {:int '(50 3)}
      {:int '(150)}

      :int-inc
      {:int '(50)}
      {:int '(51)}

      :string-take
      {:string '("abcde") :int '(3)}
      {:string '("abc") :int '()}

      :string-take
      {:string '("") :int '(3)}
      {:string '("") :int '()}

      :string-drop
      {:string '("abcde") :int '(3)}
      {:string '("de") :int '()}

      :string-drop
      {:string '("") :int '(3)}
      {:string '("") :int '()}

      )))
