(ns puj.push.config
  (:require [clojure.spec.alpha :as s]))


(s/def ::limits
  (s/keys :req-un [;; Step limit of program executions.
                   ::step
                   ;; Runtime limit of program execution (in milliseconds).
                   ::runtime
                   ;; Maximum size increase of the push-state (across all-stacks) per step of execution.
                   ::growth
                   ;; Maximum length of collections and strings.
                   ::collection-size
                   ;; Maximum magnitude of numeric values.
                   ::numeric-magnitude]))

(s/def ::push-config
  (s/keys :req [::limits]))


(def default-limits
  {:step              500
   :runtime           10000
   :growth            500
   :collection-size   1000
   :numeric-magnitude 1e12})


(defn push-config
  ([]
   {::limits default-limits})
  ([limits]
   {:post [(s/valid? ::push-config %)]}
   {::limits (merge default-limits limits)}))


(defn limit-collection
  "Limits the size of a collection, while preserving the type of the collection."
  [coll config]
  (let [e (empty coll)]
    (cond->> coll

             true
             (into e (take (:collection-size (::limits config)) coll))

             ;; `into` inserts elements into lists in reverse order.
             (list? coll)
             (reverse))))


(defn limit-string
  "Limits the length of a sting by taking the leading characters up to the limit."
  [s config]
  (apply str (take (:collection-size (::limits config)) s)))


(defn limit-number
  "Limits the magnitude of the the number `n`."
  [n config]
  (if (pos? n)
    (min n (:numeric-magnitude (::limits config)))
    (max n (- (:numeric-magnitude (::limits config))))))
