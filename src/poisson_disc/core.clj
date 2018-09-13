(ns poisson-disc.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  {})

(defn update-state [state]
  state)

(defn draw-state [state])

(defn -main [& args]
  (q/defsketch poisson-disc
    :title "poisson disc"
    :size [500 500]
    :setup setup
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [m/fun-mode]))
