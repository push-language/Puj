(ns puj.gp.genome
  (:require [clojure.spec.alpha :as spec]
            [puj.push.unit :as unit]
            [puj.util :as util]))


(spec/def ::gene (spec/or :unit-gene #(extends? unit/PushUnit %)
                          :token keyword?))
(spec/def ::genome (spec/coll-of ::gene))


; @TODO: Add support for skip gene.
(defn to-code
  [genome]
  (let [opener? #(and (map? %) (= (set (keys %)) #{:open}))] ;; {:open <n>} marks opens
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if (and (unit/push-instruction? %) (> (unit/open-count %) 0))
                             [% {:open (unit/open-count %)}]
                             [%])
                          genome)]
      (if (empty? plushy)           ;; maybe we're done?
        (if (some opener? push)     ;; done with plushy, but unclosed open
          (recur push '(:close))    ;; recur with one more close
          push)                     ;; otherwise, really done, return push
        (let [gene (first plushy)]
          (if (= gene :close)
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?) (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (:open (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open {:open (dec num-open)}])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [gene]) (rest plushy)))))))) ;; anything else


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
                #(let [gene-type (util/sample-distribution distribution)]
                   (cond
                     (= gene-type :instruction)
                     (rand-nth (keys instruction-set))

                     (= gene-type :close)
                     :close

                     (= gene-type :literal)
                     (unit/make-literal (rand-nth literals) type-set)

                     (= gene-type :erc)
                     (unit/make-literal ((rand-nth erc-generators)) type-set))))))



