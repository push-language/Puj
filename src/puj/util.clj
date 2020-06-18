(ns puj.util)


(defn ensure-seq
  "If thing is a sequential collection, returns it. Otherwise, returns it in a list."
  [thing]
  (if (sequential? thing)
    thing
    (list thing)))
