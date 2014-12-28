(ns scratch.core-test
  (:require [clojure.test :refer :all]
            [scratch.core :refer :all]))

(deftest generate-grid-test
  (testing "Generate 2x2"
    (is (=
        (generate-grid 2)
        (list '(1 1) '(1 1))
    ))))

(deftest grid-to-string-test
  (testing "Grid 2x2 to string"
    (is (=
      (grid-to-string (generate-grid 2))
      "1 1\n1 1"
    ))))

(deftest generate-down-right-cut-test
  (testing "Straight cut with 2x2 starting at position 0"
    (is (=
      (generate-down-right-cut 0 0 2)
      (list [:right :right] [:down :down] [:right :down :down] [:right :down :right] [:down :right :right] [:down :right :down] )
    ))))

(deftest split-board-1
  (testing "Right down cut"
    (is (=
      (split-board 2 [:right :down :right])
      [(list '(1 0) '(1 1)) (list '(1))]
  ))))

(deftest split-board-2
  (testing "Down right cut"
    (is (=
      (split-board 1 [:down :right])
      [(list) (list '(1))]
  ))))

(deftest split-board-random
  (testing "Other random cut"
    (is (=
      (split-board 4 [:down :right :down :right :down :down])
      [(list '(1 0) '(1 1) '(1 1)) (list '(1 1 1 1) '(0 1 1 1) '(0 0 1 1) '(0 0 1 1))]
  ))))

(deftest split-board-random-2
  (testing "Other random cut 2"
    (is (=
      (split-board 4 [:right :down :down :down :down])
      [(list '(1) '(1) '(1) '(1)) (list '(1 1 1) '(1 1 1) '(1 1 1) '(1 1 1) )]
  ))))


(deftest fits-in-grid-1
  (testing "For grid of 2x2, dimension 1 is too big"
    (is (=
      (fits-in-grid 1 1 [(list '(1 0) '(1 1)) (list '(1 1) '(1 1))])
      false
  ))))

(deftest fits-in-grid-2
  (testing "For grid of 2x2, dimension 2 is OK"
    (is (=
      (fits-in-grid 2 2 [(list '(1 0) '(1 1)) (list '(1 1) '(1 1))])
      true
  ))))

(deftest fits-in-grid-3
  (testing "For grid of 1x1, dimension 1 is OK"
    (is (=
      (fits-in-grid 1 1 [(list '(1))])
      true
  ))))

(deftest fits-in-grid-4
  (testing "For grid of 1x1, dimension 1 is not OK"
    (is (=
      (fits-in-grid 1 1 [(list '(1)) (list '(1 1))])
      false
  ))))

(deftest fits-in-grid-5
  (testing "For grid of 1x1, dimension 1 is not OK"
    (is (=
      (fits-in-grid 1 1 [(list '(1) '(1)) (list '(1))])
      false
  ))))

(deftest fits-in-grid-6
  (testing "For grid of 1x2, dimension 1 is OK"
    (is (=
      (fits-in-grid 1 2 [(list '(1) '(1)) (list '(1 1))])
      true
  ))))


(deftest extend-to-length-test
  (testing "Extend to length"
    (is (=
      (extend-to-length (list 1 1 1) 10)
      (list 1 1 1 0 0 0 0 0 0 0)
  ))))

(deftest merge-boards-test-1
  (testing "Merge boards"
    (is (=
      (merge-boards (list '(1)) (list '(1 1)) 0 0)
      (list '(2 1))
  ))))

(deftest merge-boards-test-2
  (testing "Merge boards"
    (is (=
      (merge-boards (list '(1 0) '(1 1)) (list '(1 1 1) '(0 1 1) '(0 0 1) '(0 0 1)) 1 0)
      (list '(1 1 1 1) '(1 1 1 1) '(0 0 0 1) '(0 0 0 1))
  ))))



(deftest get-possible-merge-positions-test-1
  (testing "Get possibe merge positions"
    (is (=
      (get-possible-merge-positions (list '(1 0 0) '(1 1 0) '(1 1 1)))
      (list [1 0] [2 0] [2 1])
  ))))

(deftest get-possible-merge-positions-test-2
  (testing "Get possibe merge positions"
    (is (=
      (get-possible-merge-positions (list '(1 0 0 0 0) '(1 1 1 0 0) '(1 1 1 1 0) '(1 1 1 1 1)))
      (list [1 0] [2 0] [3 0] [3 1] [4 1] [4 2])
  ))))


