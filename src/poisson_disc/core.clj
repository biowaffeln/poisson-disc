(ns poisson-disc.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [poisson-disc.generator :as generator]))

(def k 30)
(def r 10)

(defn len->grid
  "calculates the grid-width or -height depending on
  the size of the sketch"
  [length]
  (Math/ceil (/ length (/ r (Math/sqrt 2)))))

(def grid-width (len->grid 500))
(def grid-height (len->grid 500))

(defn setup []
  (q/stroke-weight 5)
  (q/stroke 255)
  {:grid (vec (repeat (* (len->grid (q/width))
                         (len->grid (q/height))) nil))
   :points []
   :active []})

(defn update-state [state]
  (if (empty? (:points state))
    state
    (generator/generate state k r [grid-width grid-height]))) ; hier kommt dann (generator state) hin

(defn draw-state [state]
  (q/background 30)
  (doseq [p (:points state)]
    (let [x (first p)
          y (second p)]
      (q/point x y))))

(defn -main [& args]
  (q/defsketch poisson-disc
    :title "poisson disc"
    :size [500 500]
    :setup setup
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
