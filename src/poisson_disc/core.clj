(ns poisson-disc.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [poisson-disc.generator :as generator]))

(def k 30) ; Number of neighbor point candidates to generate for a point each iteration
(def radius 15) ; Minimal radius between two points

(def width 1000)
(def height 600)

(defn len->grid
  "calculates the grid-col/row-count depending on the size of the sketch"
  [length]
  (-> length
      (/ (/ radius (Math/sqrt 2)))
      (Math/ceil)
      (int)))

(def grid-col-count (len->grid width))
(def grid-row-count (len->grid height))

(defn setup []
  (q/stroke-weight 5)
  (q/stroke 51)
  (let [state {:grid (vec (repeat (* (len->grid (q/width))
                                     (len->grid (q/height))) nil))
               :points []
               :active []}]
    (generator/add-point state [(rand-int width) (rand-int height)] [grid-col-count grid-row-count] [width height])))

(defn update-state [state]
  (let [next-state #(generator/generate % k radius [grid-col-count grid-row-count] [width height])]
    (nth (iterate next-state state) 10)))

(defn draw-state [state]
  (q/background 250)
  (doseq [p (:points state)]
    (let [x (first p)
          y (second p)]
      (q/point x y))))

(defn -main [& args]
  (q/defsketch poisson-disc
    :title "poisson disc"
    :size [width height]
    :setup setup
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
