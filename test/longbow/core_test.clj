(ns longbow.core-test
  (:require [clojure.test :refer :all]
            [longbow.entrypoints :refer :all]
            [longbow.ndfa :refer :all]
            [longbow.ndfa2re :refer :all]
            [longbow.ndfa-opts :refer :all]
            [longbow.utils :refer :all]
            [loom.io :refer (view)]
            [ubergraph.core :refer :all]))

(deftest a-test
  (testing "FIXED, I pass."
    (is (= 0 0))))

(deftest gen-nodes-test
  (is (= (gen-nodes (initial-graph) 0) ()))
  (is (= (gen-nodes (initial-graph) 5) '(0 1 2 3 4)))
  (is (= (gen-nodes (initial-graph 0 1 2) 5) '(3 4 5 6 7))))

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

(deftest add-ndfa-inputs-test
  (let [g (add-ndfa-inputs (initial-graph) '("ab" "cd" ""))]
    (is (= (count (nodes g)) 4))
    (is (= (count (edges g)) 5))
    (is (ndfa-isomorphic? g (ndfa-from-chains
                             [start-node "a" 1 "b" goal-node]
                             [start-node "c" 2 "d" goal-node]
                             [start-node epsilon goal-node])))))

(deftest ndfa2re-test
  (is (= (strs2res "ab") "ab"))
  (is (= (strs2res "ab" "c") "(ab|c)"))
  (is (= (strs2res "c" "ab") "(ab|c)"))
  (is (= (strs2res "ab" "cd" "") "(|ab|cd)")))

(defn -test-ndfa2re-opt [& args]
  (is (= (apply str2res-opt (butlast args)) (last args))))

(defn not* [& args]
  (not (eval args)))

(deftest ndfa2re-same-inc-out
  (let [g (ndfa-from-chains [start-node "a" 1 "x" goal-node]
                            [start-node "b" 2 "x" goal-node]
                            [start-node "a" 3 "z" goal-node]
                            [start-node "a" 4 "x" goal-node]
                            [4 "e" 4])
        same-inc #(apply -nodes-same-incoming g %)
        same-outg #(apply -nodes-same-outgoing g %)
        pairs (neq-cartesian (nodes g) (nodes g))
        eq-ins (set (filter same-inc pairs))
        eq-outs (set (filter same-outg pairs))]
    (is (= eq-ins #{[1 3] [3 1]}))
    (is (= eq-outs #{[1 2] [2 1]}))))

(deftest ndfa2re-opt-test
  (-test-ndfa2re-opt "ab" "ab")
  (-test-ndfa2re-opt "ax" "bx" "(a|b)x")
  (-test-ndfa2re-opt "AXXB" "AYYB" "AZZB" "A(XX|YY|ZZ)B")
  (-test-ndfa2re-opt "AXB" "AYB" "XB" "YB" "A?[XY]B"))

(str2res-opt :show "AXB" "AYB" "XB" "YB")

  ; DO_TEST $ (do_your_stuff 0 ["AXB", "AYB", "XB", "YB"]) == "A?[XY]B"
  ; DO_TEST $ (do_your_stuff 0 ["AXXB", "AYYB"]) == "A(XX|YY)B"
  ; DO_TEST $ (do_your_stuff 0 ["AB", "ABB", "B", "BB"]) == "A?BB?"
  ; DO_TEST $ (do_your_stuff 0 ["ABX", "AX", "BX", "X"]) == "A?B?X"
  ; DO_TEST $ (do_your_stuff 0 ["ABX", "AX", "BX", "X"]) == "A?B?X"
  ; DO_TEST $ (do_your_stuff 0 ["BC", "BCC", "BCCC"]) == "BC{1,3}"

;(str2res-opt "ab" "ac")
