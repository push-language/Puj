(ns puj.push.core-instructions-test
  (:require [clojure.test :refer :all]
            [puj.push.interpreter :as interp]
            [puj.push.instruction :as i]
            [puj.push.instruction-set :as i-set]
            [puj.test-support :as ts]))


(deftest core-instructions
  (let [ctx (interp/push-context)
        get-instr-meta (fn [instr-name] (i/instruction-meta (get-in ctx [::i-set/instruction-set instr-name])))]

    ;; To create a unit test of a push instruction, we must declare an initial push-state and an
    ;; expected push-state. The push-state returned by eval-ing the instruction on the initial push-state
    ;; should be equal to the expected push-state.

    (are
      [instr-name initial expected]
      (= (ts/stacks->state expected)
         (interp/eval-push-unit (ts/stacks->state initial) (get-instr-meta instr-name) ctx))

      :int-add       ;; Name of the instruction being tested.
      {:int '(2 1)}  ;; Initial state of stacks.
      {:int '(3)}    ;; Expected resulting state of stacks.

      :int-add       ;; Test not enough arguments
      {:int '(-29)}
      {:int '(-29)}

      :int-add       ;; Test not popping too many arguments
      {:int '(4 -92 7)}
      {:int '(-88 7)}

      :int-sub
      {:int '(3 1)}
      {:int '(-2)}

      :int-sub
      {:int '(1 10)}
      {:int '(9)}

      :int-mult
      {:int '(50 3)}
      {:int '(150)}

      ;; :int-mod
      ;; {:int '(5 33 -12)}
      ;; {:int '(3 -12)}

      ;; :int-mod
      ;; {:int '(-5 33 -12)}
      ;; {:int '(-2 -12)}

      ;; :int-mod
      ;; {:int '(5 -33 -12)}
      ;; {:int '(2 -12)}

      :int-inc
      {:int '(50)}
      {:int '(51)}

      :int-inc      ;; Test empty stack
      {:int '()}
      {:int '()}

      :int-dec
      {:int '(50 100 150)}
      {:int '(49 100 150)}

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
