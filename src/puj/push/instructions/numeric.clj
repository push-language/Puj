(ns puj.push.instructions.numeric
  (:require [puj.push.unit :as u]))


(defn instructions []
  {
   :int-add
   (with-meta
     (u/->SimpleInstruction
       [:int :int] [:int] 0
       #(vector (+ %1 %2)))
     {:puj.push.instruction/doc  "Pushes the sum of the top 2 ints onto the int stack."})

   :int-sub
   (with-meta
     (u/->SimpleInstruction
       [:int :int] [:int] 0
       #(vector (- %1 %2)))
     {:puj.push.instruction/doc  "Pushes the difference of the top 2 ints onto the int stack."})

   :int-mult
   (with-meta
     (u/->SimpleInstruction
       [:int :int] [:int] 0
       #(vector (* %1 %2)))
     {:puj.push.instruction/doc  "Pushes the product of the top 2 ints onto the int stack."})

   :int-inc
   (with-meta
     (u/->SimpleInstruction
      [:int] [:int] 0
      #(vector (inc %)))
     {:puj.push.instruction/doc "Pushes the increment of the top int onto the int stack."})

   })
