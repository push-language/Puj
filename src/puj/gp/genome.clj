(ns puj.gp.genome
  (:require [clojure.spec.alpha :as spec]
            [puj.push.unit :as unit]
            [puj.util :as util]))


(spec/def ::gene (spec/or :unit-gene #(extends? unit/PushUnit %)
                          :token keyword?))
(spec/def ::genome (spec/coll-of ::gene))


(defn generate
  [size type-set instruction-set literals erc-generators & {:keys [distribution]
                                                            :or {distribution :proportional}}]
  (let [size (if (vector? size)
               (+ (rand-int (- (apply max size) (apply min size))) (apply min size))
               size)
        distribution (if (= distribution :proportional)
                       (util/make-distribution :instruction (count instruction-set)
                                               :close (apply + (map #(unit/open-count %) (vals instruction-set)))
                                               :literal (count literals)
                                               :erc (count erc-generators))
                       (util/ensure-valid (spec/get-spec ::util/distribution) distribution))]
    (repeatedly size
                (fn [_]
                  (let [gene-type (util/sample-distribution distribution)]
                    (cond
                      (= gene-type :instruction)
                      (rand-nth (keys instruction-set))

                      (= gene-type :close)
                      :close

                      (= gene-type :literal)
                      (unit/make-literal (rand-nth literals) type-set)

                      (= gene-type :erc)
                      (unit/make-literal ((rand-nth erc-generators)) type-set)))))))



