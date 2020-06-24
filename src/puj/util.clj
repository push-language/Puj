(ns puj.util)


(defn keyword-to-str
  "Converts a keyword to a string without the colon."
  [keyword]
  (let [s (str keyword)]
    (subs s 1 (count s))))