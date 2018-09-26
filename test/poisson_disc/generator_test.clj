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
  (testing "returns the correct nearby points"
    (is (= (get-nearby-points
            [0 nil nil
             nil nil nil
             nil nil nil]
            3
            0
            [[50 50]])
           [nil nil nil nil [50 50] nil nil nil nil]))))

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
                         100)))
  (testing "invalid point"
    (is (not (point-is-valid? [50 100]
                              [nil nil nil
                               nil [50 50] nil
                               nil nil nil]
                              100)))))

(deftest find-valid-point-test
  (testing "finds the first valid point from list of points"
    (is (= (find-valid-point
            [0 nil nil
             nil nil nil
             nil nil nil]  ;grid
            [3 3]  ;grid-width/height
            [300 300]  ;width/height
            [[100 50] [200 200]] ;candidates
            [[50 50]] ;points
            100 ;r)
           [200 200])))
  (testing "chooses the first one of multiple valid points"
    (is (= (find-valid-point
            [0 nil nil
             nil nil nil
             nil nil nil]  ;grid
            [3 3]  ;grid-width/height
            [300 300]  ;width/height
            [[250 200] [100 50] [200 200]] ;candidates
            [[50 50]] ;points
            100 ;r)
           [250 200])))
  (testing "no valid point is found"
    (is (= (find-valid-point
            [0 nil nil
              nil nil nil
              nil nil nil]  ;grid
            [3 3]  ;grid-width/height
            [300 300]  ;width/height
            [[100 50] [50 150]] ;candidates
            [[50 50]] ;points
            100 ;r)
            nil))))

(run-tests)
