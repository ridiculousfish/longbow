(ns longbow.core
  (:require
   [loom.graph :refer :all]
   [loom.io :refer :all]
   [loom.attr :refer :all]
   [loom.label :refer :all]
   [taoensso.truss :as truss :refer (have have! have?)])
  (:gen-class))

(def ^:const start-node "Initial node of an NDFA" :start)
(def ^:const goal-node "Goal node of an NDFA" :goal)
(def ^:const epsilon "Empty transition" :ε)

(defn gen-nodes [g count]
  "Return 'count' new nodes for the graph g"
  (let [cur (apply max -1 (filter number? (nodes g)))
        start (+ 1 cur)]
    (range start (+ count start))))

(defn add-ndfa-input [dg input]
  "Add a string input to a directed graph"
  (let [nodecount (- (count input) 1)
        interm-nodes (gen-nodes dg nodecount)
        path (concat [start-node] interm-nodes [goal-node])
        edges (map vector path (drop 1 path))
        labels (if (empty? input) [epsilon] input)]
    (do
      (println interm-nodes)
      (println path)
      (assert (= (count edges) (count labels)))
      (as-> dg dg
        (apply add-nodes dg interm-nodes)
        (apply add-labeled-edges dg (interleave edges labels))))))

(defn add-ndfa-inputs [dg inputs]
  "Add multiple string inputs to a directed graph"
  (as-> dg dg
    (add-nodes dg start-node goal-node)
    (reduce add-ndfa-input dg inputs)))

(def dg (-> (digraph)
            (add-ndfa-inputs '("abc", "defg", ""))))
(view dg :alg :dot :fmt :png)

; (defn -main
;   "I don't do a whole lot ... yet."
;   [& args]
;   (println "Hello, World!"))
