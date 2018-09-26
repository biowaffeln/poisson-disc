(ns poisson-disc.generator-test
  (:require [clojure.test :refer :all]
            [poisson-disc.generator :refer :all]))

(deftest point->index-test
  (testing "converts to the correct index"
    (is (= (point->index
            [100 100]
            [3 3]
            [300 300])
           4))))

(deftest get-nearby-points-test
  (testing "returns a single nearby point"
    (is (= (get-nearby-points
            [0 nil nil
             nil nil nil
             nil nil nil]
            3
            0
            [[50 50]])
           [nil nil nil nil [50 50] nil nil nil nil])))
  (testing "returns correct nearby points"
    (is (= (get-nearby-points
            [0 1 2
             3 nil 4
             5 6 7]
            3
            4
            [[0 0]   [100 0]   [200 0]
             [0 100]           [200 100]
             [0 200] [100 200] [200 200]])
           [[0 0] [100 0] [200 0]
            [0 100] nil [200 100]
            [0 200] [100 200] [200 200]]))))

(deftest distance-greater-than?-test
  (testing "distance is greater"
    (is (distance-greater-than? [5 5] [0 0] 5)))
  (testing "one of the points is nil"
    (is (distance-greater-than? [5 2] nil 5)))
  (testing "distance is smaller"
    (is (not (distance-greater-than? [5 5] [0 0] 10)))))

(deftest point-is-valid-test
  (testing "valid point"
    (is (point-is-valid? [250 150]
                         [nil nil nil
                          nil [50 50] nil
                          nil nil nil]
                         100
                         [300 300])))
  (testing "point intersects with another point"
    (is (not (point-is-valid? [50 100]
                              [nil nil nil
                               nil [50 50] nil
                               nil nil nil]
                              100
                              [300 300])))))


(deftest find-valid-point-test
  (testing "finds the first valid point from list of candidates"
    (is (= (find-valid-point
            [0 nil nil
             nil nil nil
             nil nil nil]  ;grid
            [3 3]  ;grid-col/row-count
            [300 300]  ;width/height
            [[100 50] [200 200]] ;candidates
            [[50 50]] ;points
            100) ;radius
           [200 200])))
  (testing "chooses the first one of multiple valid candidates"
    (is (= (find-valid-point
            [0 nil nil
             nil nil nil
             nil nil nil]  ;grid
            [3 3]  ;grid-col/row-count
            [300 300]  ;width/height
            [[250 200] [100 50] [200 200]] ;candidates
            [[50 50]] ;points
            100) ;radius
           [250 200])))
  (testing "no valid point is found"
    (is (= (find-valid-point
            [0 nil nil
             nil nil nil
             nil nil nil]  ;grid
            [3 3]  ;grid-col/row-count
            [300 300]  ;width/height
            [[100 50] [50 150]] ;candidates
            [[50 50]] ;points
            100) ;radius
           nil))))

(run-tests)
