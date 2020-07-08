(ns puj.test-support
  (:require
    [puj.push.config :as cfg]
    [puj.push.type :as t]
    [puj.push.pushstate :as state]))


(defn stacks->state
  "A helper to create push a states with a given map of stacks."
  [stacks]
  (let [default-config (cfg/push-config)
        types (into {} (filter #(contains? stacks (key %)) (t/core-types)))
        empty-state (state/make-state types default-config)]
    (reduce #(state/set-stack %1 (key %2) (val %2))
            empty-state
            stacks)))
