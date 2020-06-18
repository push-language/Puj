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

   (with-meta
     (i/simple-instruction :int-inc [:int] :int 0 inc)
     {:puj.push.instruction/doc "Pushes the increment of the top int onto the int stack."})

   ])
