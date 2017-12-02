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
    (view g)
    (is (= (count (nodes g)) 4))
    (is (= (count (edges g)) 5))
    ))

(view (ndfa-from-chains [start-node "a" 0 "b" 1 "c" goal-node]))
