(ns puj.push.program
  (:require [clojure.spec.alpha :as spec]
            [puj.push.unit :as u]
            [puj.push.state :as state]))


(spec/def ::code (spec/and (spec/coll-of u/push-unit?) u/push-unit?))
(spec/def ::io-scheme (spec/map-of keyword? keyword?))
(spec/def ::input-scheme ::io-scheme)
(spec/def ::output-scheme ::io-scheme)


(spec/def ::trigger
  (spec/fspec :args [:old ::state/state :new ::state/state]
              :ret boolean?))
(spec/def ::revert-triggers (spec/coll-of ::trigger))
(spec/def ::stop-triggers (spec/coll-of ::trigger))


(spec/def ::program
  (spec/keys :req [::code ::input-scheme ::output-scheme]
             :opt [::revert-triggers ::stop-triggers]))


(defn make-program
  [code inputs outputs & {:keys [revert-triggers stop-triggers]}]
  {::code            code
   ::input-scheme    inputs
   ::output-scheme   outputs
   ::revert-triggers revert-triggers
   ::stop-triggers   stop-triggers})


(spec/def ::inputs (spec/map-of keyword? any?))
(spec/def ::outputs (spec/map-of keyword? any?))

(spec/def ::program-result
  (spec/keys :req [::program ::inputs ::outputs]))


(defn make-program-result
  [program inputs outputs & {:keys [cache] :or {cache {}}}]
  (merge cache
         {::program program
          ::inputs inputs
          ::outputs outputs}))