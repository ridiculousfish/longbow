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
(def ^:const viz viz-graph)

(defn initial-graph [& args]
  "Return a starting (empty) graph"
  (apply multidigraph start-node goal-node args))

(defn gen-nodes [g count]
  "Return 'count' new nodes for the graph g"
  (let [cur (apply max -1 (filter number? (nodes g)))
        start (+ 1 cur)]
    (range start (+ count start))))

(defn -add-path [dg nodes labels]
  "Given a list of nodes and a list of labels used to form the edges, add nodes to the graph"
  (let [_ (assert (= (count nodes) (inc (count labels))) "Nodes should be 1 more than labels")
        attrs (map (partial hash-map :label) labels)
        edges (map vector nodes (rest nodes) attrs)]
    (apply add-directed-edges dg edges)))

(defn -add-ndfa-input [dg input]
  "Add a string input to a directed graph"
  (let [nodecount (- (count input) 1)
        interm-nodes (gen-nodes dg nodecount)
        path (concat [start-node] interm-nodes [goal-node])
        labels (if (empty? input) [epsilon] input)
        attrs (map (partial hash-map :label) labels)
        newedges (map vector path (drop 1 path) attrs)]
    (do
      (println newedges)
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
        newedges (map relabel-edge oldedges)]
    (as-> dg dg
      (apply remove-edges dg oldedges)
      (apply add-directed-edges dg newedges))))

(defn add-ndfa-inputs [dg inputs]
  "Add multiple string inputs to a directed graph"
  (as-> dg dg
    (add-nodes dg start-node goal-node)
    (reduce -add-ndfa-input dg inputs)))

(defn ndfa-from-chains [& chains]
  "Specify a graph via a list of chains. A chain is a sequence (node label node label node...)"
  (let [apply-chain (fn [dg chain] ()
                      (let [nodes (take-nth 2 chain)
                            labels (take-nth 2 (rest chain))]
                        (assert (odd? (count chain)) "Chain length should be odd")
                        (assert (not-any? number? labels) "Labels should not be numbers")
                        (-add-path dg nodes labels)))]
    (reduce apply-chain (initial-graph) chains)))
