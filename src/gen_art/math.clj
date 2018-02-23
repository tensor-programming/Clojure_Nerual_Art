(ns gen-art.math)

(defn vec*
  ([a] a)
  ([a b] (map * a b))
  ([a b & more] (reduce vec* (vec* a b) more)))

(defn vec-
  ([a] a)
  ([a b] (map - a b))
  ([a b & more] (reduce vec- (vec- a b) more)))

(defn dot [v1 v2]
  (reduce + (vec* v1 v2)))

(defn matrix* [m v]
  (map #(dot v %) m))

(defn abs-value [x]
  (if (< x 0) (- x) x))

(defn rand-vec [n]
  (repeatedly n #(if (< (rand) 0.5) (rand) (- (rand)))))

(defn rand-matrix [n m]
  (repeatedly n #(rand-vec m)))

(defn sig [x]
  (/ 1 (+ 1 (Math/exp (- (* x 5))))))

;;1 / (1 + e^-x)

(defn tanh [x]
  (- (* 2 (sig (* 2 x))) 1))

;;(2 * sig * 2 * x) - 1
