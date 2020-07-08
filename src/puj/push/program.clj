(ns puj.push.program
  (:require [clojure.spec.alpha :as spec]
            [puj.push.unit :as u]
            [puj.push.type :as t]
            [puj.push.config :as config]
            [clojure.spec.alpha :as s]))


(spec/def ::code (spec/and (spec/coll-of u/push-unit?) u/push-unit?))
(spec/def ::io-scheme (spec/map-of keyword? ::t/type-name))
(spec/def ::input-scheme ::io-scheme)
(spec/def ::output-scheme ::io-scheme)


(spec/def ::signature
  (spec/keys :req [::input-scheme ::output-scheme ::config/push-config]))


(spec/def ::program
  (spec/keys :req [::code ::signature]))


(defn make-program
  "Creates a Push program.

  Programs are made up of two parts: its `code` and its `signature`.

  Callers must supply the `code` and can either supply an entire `signature` or all of its parts.

  Signatures are made up of
  - The `inputs` scheme of the program as a map of input name to stack name.
  - The `outputs` scheme of the program as a map of output name to stack name.
  - An optional custom `push-config` to use when executing the program.
  "
  ([code inputs outputs & {:keys [push-config]
                           :or   {push-config (config/push-config)}}]
   (make-program code
                 {::input-scheme       inputs
                  ::output-scheme      outputs
                  ::config/push-config push-config}))
  ([code signature]
   {:post [(s/valid? ::program %)]}
   {::code      code
    ::signature signature}))


(defn arity
  "The arity of a push program."
  [program]
  (count (get-in program ::signature ::input-scheme)))


(spec/def ::inputs (spec/map-of keyword? any?))

(spec/def ::outputs (spec/map-of keyword? any?))

(spec/def ::program-result
  (spec/keys :req [::program ::inputs ::outputs]
             :opt-un [::cache]))


(defn make-program-result
  "The result of running a program.

  Holds:
  - the `program` that was run, including its signature.
  - the `inputs` that were passed to the `program`.
  - the `outputs` produced by the program.
  - an optional `cache` of other attributes gathered during program execution.

  The program result contains all the information necessary to evaluate and analyze the program. This includes
  non-functional attributes stored in the `cache`, such as runtime.
  "
  [program inputs outputs & {:keys [cache] :or {cache {}}}]
  {::program program
   ::inputs  inputs
   ::outputs outputs
   :cache    cache})
