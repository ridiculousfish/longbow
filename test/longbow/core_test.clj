(ns longbow.core-test
  (:require [clojure.test :refer :all]
            [longbow.ndfa :refer :all]
            [loom.io :refer (view)]
            [ubergraph.core :refer :all]))

(deftest a-test
  (testing "FIXED, I pass."
    (is (= 0 0))))

(deftest gen-nodes-test
  (is (= (gen-nodes (initial-graph) 0) ()))
  (is (= (gen-nodes (initial-graph) 5) '(0 1 2 3 4)))
  (is (= (gen-nodes (initial-graph 0 1 2) 5) '(3 4 5 6 7))))

(deftest add-ndfa-inputs-test
  (let [g (add-ndfa-inputs (initial-graph) '("ab" "cd" ""))]
    (is (= (count (nodes g)) 4))
    (is (= (count (edges g)) 5))))

(deftest isomorphism-test
  (let [iso? ndfa-isomorphic?
        not-iso? (complement ndfa-isomorphic?)
        make ndfa-from-chains]
    (is (iso? (make [start-node "a" 0 "b" 1 "c" goal-node])
              (make [start-node "a" 0 "b" 1 "c" goal-node])))
    (is (iso? (make [start-node "a" 3 "b" 5 "c" goal-node] [3 "x" 5] [7])
              (make [30 "x" 50] [start-node "a" 30 "b" 50 "c" goal-node] [70])))
    (is (not-iso? (make [start-node "a" 3 "b" 5 "c" goal-node] [3 "x" 5] [7])
                  (make [30 "x" 50] [start-node "a" 0 "b" 10 "c" goal-node] [70])))
    (is (not-iso? (make [start-node "a" 100 "b" goal-node]) (make [start-node "b" 100 "a" goal-node])))))

;(view (ndfa-from-chains [start-node "a" 0 "b" 1 "c" goal-node]))
