(ns puj.push.instructions.numeric
  (:require [puj.push.instruction :as i]))


(defn instructions []
  [
   (with-meta
     (i/simple-instruction
       :int-add             ; The name of the instruction.
       [:int :int]          ; Takes two arguments from the :int stack
       :int                 ; Pushes a single result to the :int stack.
       0                    ; The instruction opens 0 code blocks.
       +)                   ; The function which turns arguments to results.
     {:puj.push.instruction/doc  "Pushes the sum of the top 2 ints onto the int stack."})

   (with-meta
     (i/simple-instruction :int-sub [:int :int] :int 0 -)
     {:puj.push.instruction/doc  "Pushes the difference of the top 2 ints onto the int stack."})

   (with-meta
     (i/simple-instruction :int-mult [:int :int] :int 0 *)
     {:puj.push.instruction/doc  "Pushes the product of the top 2 ints onto the int stack."})

   ; @TODO :int-div - what kind of protected division should we use? push-like, which would no-op if divide by zero, or analytic quotient, as described here: https://push-language.hampshire.edu/t/gptp-2019-notes/

   (with-meta
     (i/simple-instruction :int-mod [:int :int] :int 0 mod)
     {:puj.push.instruction/doc "Pushes the mod of the top 2 ints onto the int stack."})

   (with-meta
     (i/simple-instruction :int-inc [:int] :int 0 inc)
     {:puj.push.instruction/doc "Pushes the increment of the top int onto the int stack."})

   (with-meta
     (i/simple-instruction :int-dec [:int] :int 0 dec)
     {:puj.push.instruction/doc "Pushes the decrement of the top int onto the int stack."})

   ])
