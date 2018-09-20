(ns poisson-disc.generator)

(defn generate
  "generates next iteration of poisson disc algorithm"
  [state k r [grid-width grid-height] [width heigth]]
  (let [active (seq (:active state))
        points (rand-points active k r)]))

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

(defn point-is-valid
  "Checks if a point doesn't intersect with other points on the grid.
  If the point is valid, return point, else return nil"
  [point grid r points]
  ())

(defn point->grid
  "Gets the index of a point inside the grid"
  [[x y] [grid-width grid-height] [width heigth]]
  (let [grid-x (* grid-width (/ x width))
        grid-y (* grid-height (/ y heigth))]
    (+ (* grid-width y) x)))

(defn get-nearby-points
  "Returns all adjecent points for the points in the cell of index"
  [grid grid-width index points]
  [(get points (get grid (- (dec index) grid-width)))
   (get points (get grid (- index) grid-width))
   (get points (get grid (- (inc index) grid-width)))
   (get points (get grid (dec index)))
   (get points (get grid index))
   (get points (get grid (inc index)))
   (get points (get grid (+ (dec index) grid-width)))
   (get points (get grid (+ index) grid-width))
   (get points (get grid (+ (inc index) grid-width)))])

(defn distance-acceptable?
  [point1 point2]
  ())