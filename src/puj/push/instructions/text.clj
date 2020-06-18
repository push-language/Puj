(ns puj.push.instructions.text
  (:require [puj.push.instruction :as i]))


(defn instructions []
  [
   (with-meta
     (i/simple-instruction
       :string-take [:string :int] :string 0
       #(apply str (take %2 %1)))
     {:puj.push.instruction/doc "Pushes the first `n` characters of the top string as a new string."})

   (with-meta
     (i/simple-instruction
       :string-drop [:string :int] :string 0
       #(apply str (drop %2 %1)))
     {:puj.push.instruction/doc "Pushes the first `n` characters of the top string as a new string."})
  ])
