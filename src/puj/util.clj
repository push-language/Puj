(ns puj.util
  (:require [clojure.spec.alpha :as spec]))


(defn keyword-to-str
  "Converts a keyword to a string without the colon."
  [keyword]
  (let [s (str keyword)]
    (subs s 1 (count s))))


(defn ensure-valid
  [spec value]
  (if (spec/valid? spec value)
    value
    (throw (AssertionError. (spec/explain-str spec value)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Distribution Utility


(spec/def ::distribution (spec/and (spec/map-of any? number?)
                                   (fn [d] (= (float (apply + (vals d))) 1.0))))

(defn make-distribution
      [& args]
      (let [pairs (partition 2 args)
            elements (map first pairs)
            probabilities (map second pairs)
            total-prob (apply + probabilities)
            normalized (map #(/ % total-prob) probabilities)]
        (zipmap elements normalized)))


(defn sample-distribution
  ([distribution] (first (sample-distribution distribution 1)))
  ([distribution n]
   (ensure-valid (spec/get-spec ::distribution) distribution)
   (let [pairs (seq distribution)
         cumulative (reductions + (map second pairs))]
     (map (fn [r] (first (nth pairs (count (filter #(< % r) cumulative)))))
          (repeatedly n rand)))))
