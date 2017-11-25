(ns longbow.core
  (:require
   [loom.graph :refer :all]
   [loom.io :refer :all]
   [loom.attr :refer :all]
   [loom.label :refer :all])
  (:gen-class))

; (defn -main
;   "I don't do a whole lot ... yet."
;   [& args]
;   (println "Hello, World!"))

(defn gen-nodes [g count]
  "Return 'count' new nodes for the graph g"
  (let [cur (apply max -1 (nodes g))
        start (+ 1 cur)]
    (range start (+ count start))))

(defn str2ndfa [input]
  (let [edgecount (count input)
        nodecount (+ 1 edgecount)
        dg (digraph)
        nodes (gen-nodes dg nodecount)
        edges (map vector nodes (drop 1 nodes))
        labels input
        goalnode (- nodecount 1)
        dg (apply add-nodes dg nodes)
        dg (apply add-labeled-edges dg (interleave edges labels))
        dg (add-label dg 0 "start 0")
        dg (add-label dg goalnode (str "goal " goalnode))]
    dg))

(view (str2ndfa "abcde") :alg :dot :fmt :png)
