(ns longbow.ndfa
  (:require
   [loom.graph]
   [loom.io :refer :all]
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

(defn -add-ndfa-input [dg input]
  "Add a string input to a directed graph"
  (let [nodecount (- (count input) 1)
        interm-nodes (gen-nodes dg nodecount)
        path (concat [start-node] interm-nodes [goal-node])
        labels (if (empty? input) [epsilon] input)
        attrs (map (partial hash-map :label) labels)
        newedges (map vector path (drop 1 path) attrs)]
    (do
      (assert (= (count newedges) (count labels)))
      (as-> dg dg
        (apply add-nodes dg path)
        (apply add-directed-edges dg newedges)))))

(defn relabel [relabeler dg]
  "Relabel edge labels"
  (let [relabel-edge (fn [edge]
                       (let [oldattrs (attrs dg edge)
                             newattrs (update oldattrs :label relabeler)]
                          [(:src edge) (:dest edge) newattrs]))
        oldedges (edges dg)
        newedges (map relabel-edge oldedges)
        _ (println "relabel old edges: " oldedges)
        _ (println "relabel new edges: " newedges)]
    (as-> dg dg
      (apply remove-edges dg oldedges)
      (apply add-directed-edges dg newedges))))

(defn add-ndfa-inputs [dg inputs]
  "Add multiple string inputs to a directed graph"
  (as-> dg dg
    (add-nodes dg start-node goal-node)
    (reduce -add-ndfa-input dg inputs)))
