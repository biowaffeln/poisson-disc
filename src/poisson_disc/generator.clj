(ns poisson-disc.generator)

(defn generate
  "generates next iteration of poisson disc algorithm"
  [state k r [grid-width grid-height] [width heigth]]
  (let [active (seq (:active state))
        points (rand-points active k r)
        grid (seq (:grid state))]
  [active points grid]))

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

(defn distance-smaller-than?
  "returns true if the distance between two points is smaller than
  the distance d"
  [[x1 y1] [x2 y2] d]
  (< (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2))) d))

(defn point-is-valid
  "Checks if a point doesn't intersect with other points on the grid.
  If the point is valid, return point, else return nil"
  [point r points]
  [true])

(defn find-valid-point
  "TODO: MICHI"
  [grid [grid-width grid-height] [width heigth] points]
  (for [point points 
        :when (point-is-valid point [4 4])]
    [point]))

;(defn find-valid-point
;  "TODO: MICHI"
;  [grid [grid-width grid-height] [width heigth] points]
;  (loop [point points]
;    [(loop [candidate (get-nearby-points grid grid-width (point->index point [grid-width grid-height] [width heigth]) points)]
;      [candidate])]))

;(find-valid-point [[1 4] [1 5]])
