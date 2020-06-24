(ns puj.push.instructions.text
  (:require [puj.push.unit :as u]))


(defn instructions []
  {
   :string-take
   (with-meta
     (u/simple-instruction
       [:string :int] [:string] 0
       #(vector (apply str (take %2 %1))))
     {:puj.push.instruction/doc "Pushes the first `n` characters of the top string as a new string."})

   :string-drop
   (with-meta
     (u/simple-instruction
       [:string :int] [:string] 0
       #(vector (apply str (drop %2 %1))))
     {:puj.push.instruction/doc "Pushes the first `n` characters of the top string as a new string."})

   })
