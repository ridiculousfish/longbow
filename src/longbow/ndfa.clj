(ns longbow.ndfa
  (:require
   [longbow.utils :refer :all]
   [loom.graph]
   [loom.io :refer :all]
   [ubergraph.core :refer :all]
   (:gen-class)))

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

(defn label-of [dg edge]
  "Return the label of a given edge"
  (:label (attrs dg edge)))

(defn make-label-attr [label]
  "return an attribute dictionary for a label"
  (hash-map :label label))

(defn -add-path [dg nodes labels]
  "Given a list of nodes and a list of labels used to form the edges, add nodes to the graph"
  (let [_ (assert (= (count nodes) (inc (count labels))) "Nodes should be 1 more than labels")
        attrs (map make-label-attr labels)
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
                            destring (fn [x]
                                       "Convert a string to its first character"
                                       (if (string? x)
                                         (first-and-only x)
                                         x))
                            labels (map destring (take-nth 2 (rest chain)))]
                        (assert (odd? (count chain)) "Chain length should be odd")
                        (assert (not-any? number? labels) "Labels should not be numbers")
                        (-add-path dg nodes labels)))]
    (reduce apply-chain (initial-graph) chains)))

(defn labels-between [dg node1 node2]
  "edge labels from node1 to node2"
  (map (partial label-of dg) (find-edges dg node1 node2)))

(defn sorted-labels-between [dg node1 node2]
  "sorted labels from node to node2"
  (sort (labels-between dg node1 node2)))

(defn self-labels [dg node]
  "return labels for self-edges"
  (labels-between dg node node))

(defn incoming-nedges [dg node]
  "Returns collection of incoming [node label], neglecting self-edges"
  (set (for [inc (in-edges dg node)
             :let [inc-node (src inc)]
             :when (not= node inc-node)]
         [inc-node (label-of dg inc)])))

(defn outgoing-nedges [dg node]
  "Returns collection of outgoing [node label], neglecting self-edges"
  (set (for [out (out-edges dg node)
             :let [out-node (dest out)]
             :when (not= node out-node)]
         [out-node (label-of dg out)])))

(defn remove-incoming-nedges [dg node nedges]
  "Removes incoming nedges"
  (let [remove-1-nedge
        (fn [g [incnode label]]
          (remove-edges g (find-edge g {:src incnode :dst node :label label})))]
    (reduce #(remove-1-nedge %1 node %2) dg nedges)))

(defn incoming-nedgeset [dg node]
  "Return collection of incoming [node, set(labels)], neglecting self-edges."
  (zipmapset (incoming-nedges dg node)))

(defn nedge-label [nedge]
  "return the label of a nedge"
  (second nedge))

(defn nedge-node [nedge]
  "return the node of a nedge"
  (first nedge))

(defn ndfa-isomorphic? [g1 g2]
  "Returns if the two NDFAs are isomorphic, respecting edge labels"
  (letfn [(can-assign [nodemap [node1 node2]]
            "Return whether the assignment node1 -> node2 is valid in nodemap. node1 s in g1, node2 is in g2"
            (assert (not (contains? nodemap node1)) "node is already in nodemap")
            (assert (has-node? g1 node1) "node1 not in g1")
            (assert (has-node? g2 node2) "node2 not in g2")
            (and
             (all? (for [succ1 (successors g1 node1)
                         :let [succ2 (nodemap succ1)]
                         :when succ2]
                     (= (sorted-labels-between g1 node1 succ1) (sorted-labels-between g2 node2 succ2))))
             (all? (for [pred1 (predecessors g1 node1)
                         :let [pred2 (nodemap pred1)]
                         :when pred2]
                     (= (sorted-labels-between g1 pred1 node1) (sorted-labels-between g2 pred2 node2))))))

          (try-assign [[nodemap rem1 rem2] node1 node2]
                      (when (can-assign nodemap [node1 node2])
                        [(assoc nodemap node1 node2) (disj rem1 node1) (disj rem2 node2)]))

          (subiso [[nodemap rem1 rem2 :as assn]]
                  (if (empty? rem1)
                    (do (assert (empty? rem2) "Nodes should have same length")
                        true)
                    (some boolean (for [[node1 node2] (cartesian rem1 rem2)
                                        :let [subassn (try-assign assn node1 node2)]
                                        :when subassn]
                                    (subiso subassn)))))]
    (assert (has-node? g1 start-node) "Missing start node")
    (assert (has-node? g1 goal-node) "Missing goal node")
    (assert (has-node? g2 start-node) "Missing start node")
    (assert (has-node? g2 goal-node) "Missing goal node")
    (boolean (some-> [{} (set (nodes g1)) (set (nodes g2))]
                     (try-assign start-node start-node)
                     (try-assign goal-node goal-node)
                     (subiso)))))
