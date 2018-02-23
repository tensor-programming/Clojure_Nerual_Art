(ns gen_art.core
  (:require [quil.core :as q :refer :all] )
  (:require [gen-art.math :refer :all])
  (:require [gen-art.network :refer :all]))

(def res-x 500)
(def res-y 500)
(def steps 200)

(defn xfn [x] (Math/cos (* Math/PI x)))
(defn yfn [y] (Math/sin (* Math/PI y)))

(def inputs  [
  [1 1 (xfn 1) (yfn 1)] 
  [1 0 (xfn 1) (yfn 0)]
  [1 1 (xfn 0) (yfn 1)] 
  [0 0 (xfn 0) (yfn 0)]])
(def outputs [[0 0 0]   [1 1 1]  [1 1 1]   [0 0 0]])

(defn rand-net []
  [(layer 4 10 tanh false)
   (layer 20 20 tanh true)
   (layer 20 20 tanh true)
   (layer 20 20 tanh true)
   (layer 10 3 sig true)])

(defn trained-net []
  (hill-climb (rand-net) inputs outputs steps))

(defn draw-fn [x y net]
  (let [x-norm (/ x res-x)
        y-norm (/ y res-y)
        [r g b] (feed-forward net [x-norm y-norm (xfn x-norm) (yfn y-norm)])]
    [(* r 255) (* g 255) (* b 255)]))

(defn setup []
  (q/background 255)
  (q/stroke-weight 2))

(defn draw []
  (let [net (trained-net)]
    (doseq [x (range res-x) y (range res-y)]
      (do
        (let [[r g b] (draw-fn x y net)]
          (q/stroke-float r g b))
        (q/point x y)))
    (save-frame "render-#####.png")))

(q/defsketch leaf
  :host "host"
  :size [res-x res-y]
  :setup setup
  :draw draw)

(defn -main [& args])
