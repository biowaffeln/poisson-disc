(ns poisson-disc.generator)

(defn rand-point
  "Generate a point chosen randomly from the
    spherical annulus between radius r and 2r around active"
  [[x y] r]
  (let [radius (+ r (rand r))
        angle (rand (* 2 Math/PI))]
    [(+ (* (Math/sin angle) radius) x)
     (+ (* (Math/cos angle) radius) y)]))

(defn rand-points
  "Returns lazy sequence of k random points"
  [active k r]
  (repeatedly k #(rand-point active r)))

(defn point->index
  "Gets the index of a point inside the grid"
  [[x y] [grid-width grid-height] [width height]]
  (let [grid-x (Math/floor (* grid-width (/ x width)))
        grid-y (Math/floor (* grid-height (/ y height)))]
    (int (+ (* grid-width grid-y) grid-x))))

(defn get-nearby-points
  "Returns all adjecent points for the points in the cell of index"
  [grid grid-width index points]
    [(get points (get grid (- (dec index) grid-width)))
     (get points (get grid (- index grid-width)))
     (get points (get grid (- (inc index) grid-width)))
     (get points (get grid (dec index)))
     (get points (get grid index))
     (get points (get grid (inc index)))
     (get points (get grid (+ (dec index) grid-width)))
     (get points (get grid (+ index grid-width)))
     (get points (get grid (+ (inc index) grid-width)))])

(defn distance-greater-than?
  "returns true if the distance between two points is greater than
  the distance d, else return false"
  [p1 p2 distance]
  (if (and p1 p2)
    (let [x1 (first p1)
          y1 (second p1)
          x2 (first p2)
          y2 (second p2)]
      (> (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2))) distance))
    true))

(defn point-is-valid?
  "Checks if a point doesn't intersect with other points.
  If the point is valid, return true, else return false"
  [point points distance]
  (every? #(distance-greater-than? point % distance) points))

(defn find-valid-point
  "returns the first valid point, if there aren't any returns nil"
  [grid [grid-width grid-height] [width height] candidates points r]
  (loop [candidate (first candidates)
         remaining (rest candidates)]
    (when candidate
      (let [grid-index (point->index candidate [grid-width grid-height] [width height])
            nearby-points (get-nearby-points grid grid-width grid-index points)]
        (if (point-is-valid? candidate nearby-points r)
          candidate
          (recur (first remaining) (rest remaining)))))))

(defn add-point
  "Takes the state and a point and adds the point to points,
  active and the point index to grid"
  [state point [grid-width grid-height] [width height]]
  (let [grid-index (point->index point [grid-width grid-height] [width height])
        point-index (count (:points state))]
    (-> state
        (update-in [:points] conj point)
        (update-in [:active] conj point)
        (assoc-in [:grid grid-index] point-index))))

(defn generate
  "generates next iteration of poisson disc algorithm"
  [state k r [grid-width grid-height] [width height]]
  (let [active (first (:active state))
        points (:points state)
        candidates (rand-points active k r)
        grid (:grid state)
        new-point (find-valid-point grid [grid-width grid-height] [width height] candidates points r)]
    (if new-point
      (add-point state new-point [grid-width grid-height] [width height])
      (update-in state [:active] subvec 1))))
