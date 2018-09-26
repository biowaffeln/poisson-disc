(ns poisson-disc.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [poisson-disc.generator :as generator]))

(def k 30) ; Number of neighbor point candidates to generate for a point each iteration
(def radius 20) ; Minimal radius between two points

(defn len->grid
  "calculates the grid-col/row-count depending on the size of the sketch"
  [length]
  (-> length
      (/ (/ radius (Math/sqrt 2)))
      (Math/ceil)
      (int)))

(def grid-col-count (len->grid 500))
(def grid-row-count (len->grid 500))

(defn setup []
  (q/stroke-weight 5)
  (q/stroke 51)
  (let [state {:grid (vec (repeat (* (len->grid (q/width))
                                     (len->grid (q/height))) nil))
               :points []
               :active []}]
    (generator/add-point state [250 250] [grid-col-count grid-row-count] [500 500])))

(defn update-state [state]
  (if (empty? (:active state))
    state
    (generator/generate state k radius [grid-col-count grid-row-count] [500 500])))

(defn draw-state [state]
  (q/background 250)
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
