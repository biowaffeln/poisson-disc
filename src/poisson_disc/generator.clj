(ns poisson-disc.generator)

(defn rand-point
  "Returns a point chosen randomly from the spherical annulus between radius r and 2radius around active"
  [[x y] radius]
  (let [radius (+ radius (rand radius))
        angle (rand (* 2 Math/PI))]
    [(+ (* (Math/sin angle) radius) x)
     (+ (* (Math/cos angle) radius) y)]))

(defn rand-points
  "Returns lazy sequence of k random points"
  [active-point k radius]
  (repeatedly k #(rand-point active-point radius)))

(defn point->index
  "Returns the index of a given point inside the grid"
  [[x y] [grid-col-count grid-row-count] [width height]]
  (let [grid-x (Math/floor (* grid-col-count (/ x width)))
        grid-y (Math/floor (* grid-row-count (/ y height)))]
    (int (+ (* grid-col-count grid-y) grid-x))))

(defn get-nearby-points
  "Returns all adjacent points for the points in the cell of index"
  [grid grid-col-count index points]
  [(get points (get grid (- (dec index) grid-col-count)))
   (get points (get grid (- index grid-col-count)))
   (get points (get grid (- (inc index) grid-col-count)))
   (get points (get grid (dec index)))
   (get points (get grid index))
   (get points (get grid (inc index)))
   (get points (get grid (+ (dec index) grid-col-count)))
   (get points (get grid (+ index grid-col-count)))
   (get points (get grid (+ (inc index) grid-col-count)))])

(defn in-bounds?
  "Returns true if point is within the sketch boundaries, else returns false"
  [[x y] [width height]]
  (and (>= x 0)
       (>= y 0)
       (< x width)
       (< y height)))

(defn distance-greater-than?
  "Returns true if the distance between two points is greater than the distance, else returns false"
  [p1 p2 distance]
  (if (and p1 p2)
    (let [x1 (first p1)
          y1 (second p1)
          x2 (first p2)
          y2 (second p2)]
      (> (Math/sqrt (+ (Math/pow (- x2 x1) 2) (Math/pow (- y2 y1) 2))) distance))
    true))

(defn point-is-valid?
  "Returns true if a point is within the bounds of the sketch and doesn't intersect with other points,
  else returns false"
  [point points distance [width height]]
  (and (in-bounds? point [width height])
       (every? #(distance-greater-than? point % distance) points)))

(defn find-valid-point
  "Returns the first valid point, if there aren't any returns nil"
  [grid [grid-col-count grid-row-count] [width height] candidates points radius]
  (loop [candidate (first candidates)
         remaining (rest candidates)]
    (when candidate
      (let [grid-index (point->index candidate [grid-col-count grid-row-count] [width height])
            nearby-points (get-nearby-points grid grid-col-count grid-index points)]
        (if (point-is-valid? candidate nearby-points radius [width height])
          candidate
          (recur (first remaining) (rest remaining)))))))

(defn add-point
  "Takes the state and a point and adds the point to points, active and the point index to grid"
  [state point [grid-col-count grid-row-count] [width height]]
  (let [grid-index (point->index point [grid-col-count grid-row-count] [width height])
        point-index (count (:points state))]
    (-> state
        (update-in [:points] conj point)
        (update-in [:active] conj point)
        (assoc-in [:grid grid-index] point-index))))

(defn drop-nth [coll n]
  (vec (concat
        (take n coll)
        (drop (inc n) coll))))

(defn generate
  "Generates next iteration of poisson disc algorithm"
  [state k radius [grid-col-count grid-row-count] [width height]]
  (if (empty? (:active state))
    state
    (let [random-index (rand-int (count (:active state)))
          active-point (nth (:active state) random-index)
          points (:points state)
          candidates (rand-points active-point k radius)
          grid (:grid state)
          new-point (find-valid-point grid [grid-col-count grid-row-count] [width height] candidates points radius)]
      (if new-point
        (add-point state new-point [grid-col-count grid-row-count] [width height])
        (update-in state [:active] drop-nth random-index)))))
