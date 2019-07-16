(ns puj.push.instructions.code
  (:require [puj.push.unit :as u]))


(defn instructions []
  {
   :no-op
   (with-meta
     (u/state-to-state-instruction [] 0 identity)
     {:puj.push.instruction/doc  "Does nothing."})

   :no-op-open
   (with-meta
     (u/state-to-state-instruction [] 1 identity)
     {:puj.push.instruction/doc  "Does nothing except open a code block."})
   })