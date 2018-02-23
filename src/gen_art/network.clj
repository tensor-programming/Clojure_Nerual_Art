(ns gen-art.network
  (:require [gen-art.math :refer :all]))

(defrecord layer-record [matrix function bias-enabled])

(defn layer [inputs neurons function bias-enabled]
  (if bias-enabled
    (layer-record. (rand-matrix neurons (+ inputs 1)) function bias-enabled)
    (layer-record. (rand-matrix neurons inputs) function bias-enabled)))

(defn feed-forward [net input]
  (loop [net net input input i 0]
    (if (>= i (count net))
      input
      (let [curr-layer (nth net i)]
        (if (:bias-enabled curr-layer)
          (recur net (map (:function curr-layer)
                          (matrix* (:matrix curr-layer) (concat input [-1]))) (+ i 1))
          (recur net (map (:function curr-layer)
                          (matrix* (:matrix curr-layer) input)) (+ i 1)))))))

(defn mutate-layer [layer-in mutation-chance]
  (if (< (rand) mutation-chance)
    (let [n (count (:matrix layer-in))
          m (count (nth (:matrix layer-in) 0))
          target (rand-int n)]
      (layer-record.
       (for [i (range n)]
         (if (= i target)
           (rand-vec m)
           (nth (:matrix layer-in) i)))
       (:function layer-in)
       (:bias-enabled layer-in)))
    layer-in))

(defn mutate [net mutation-chance]
  (map #(mutate-layer % mutation-chance) net))

(defn score [net input expected-output]
  (let [output (map #(feed-forward net %) input)
        result (reduce + (map abs-value (flatten (map vec- expected-output output))))]
    [(* result result) net]))

(defn hill-climb [net input expected-output iterations]
  (loop [net net
         input input
         expected-output expected-output
         iterations iterations
         i 0]
    (if (= iterations i)
      net
      (let [gen (repeatedly 500 #(mutate net 0.33))
            result (first (sort-by first
                                   (pmap #(score % input expected-output) gen)))]
        (if (= (mod i 10) 0) (println (double (/ i iterations)) (first result)))
        (recur (second result) input expected-output iterations (+ i 1))))))
