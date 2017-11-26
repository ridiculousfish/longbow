(ns longbow.ndfa
  (:require
   [ubergraph.core :refer :all]
   [taoensso.truss :as truss :refer (have have! have?)])
  (:gen-class))

(def ^:const start-node "Initial node of an NDFA" :start)
(def ^:const goal-node "Goal node of an NDFA" :goal)
(def ^:const epsilon "Empty transition" :ε)

(defn initial-graph []
  "Return a starting (empty) graph"
  (multidigraph))

(defn gen-nodes [g count]
  "Return 'count' new nodes for the graph g"
  (let [cur (apply max -1 (filter number? (nodes g)))
        start (+ 1 cur)]
    (range start (+ count start))))

(defn -add-labeled-edges [dg edges labels]
  "Adds edges [x y] with labels labels"
  (let [edgelabels (map #(conj %1 {:label %2}) edges labels)]
    (add-directed-edges dg edgelabels)))

(defn -add-ndfa-input [dg input]
  "Add a string input to a directed graph"
  (let [nodecount (- (count input) 1)
        interm-nodes (gen-nodes dg nodecount)
        path (concat [start-node] interm-nodes [goal-node])
        edges (map vector path (drop 1 path))
        labels (if (empty? input) [epsilon] input)]
    (do
      (assert (= (count edges) (count labels)))
      (as-> dg dg
        (apply add-nodes dg interm-nodes)
        (-add-labeled-edges dg edges labels)))))

; (defn relabel [relabeler dg]
;   "Relabel edge labels"
;   (let [edges (edges dg)
;         oldlabels (map (partial label-of dg) edges)
;         newlabels (map relabeler oldlabels)]
;     (as-> dg newg
;       (remove-edges* dg edges)
;       (-add-labeled-edges dg edges newlabels))))
(defn relabel [relabeler dg] (dg))
(+ 1 2)

(defn add-ndfa-inputs [dg inputs]
  "Add multiple string inputs to a directed graph"
  (as-> dg dg
    (add-nodes dg start-node goal-node)
    (reduce -add-ndfa-input dg inputs)))

