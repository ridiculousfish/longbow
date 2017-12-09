(ns longbow.ndfa-opts
  (:require
   [longbow.utils :refer :all]
   [longbow.ndfa :refer :all]
   [loom.graph]
   [loom.io :refer :all]
   [ubergraph.core :refer :all])
  (:gen-class))

(defrecord Queryable [graph node edges])

(defn select [dg node]
  "Return a queryable for the given graph and node"
  (Queryable. dg node ()))

(defn labels [q]
  "Labels for a queryable"
  (map (partial label-of (q :graph)) (q :edges)))

(defn -add-outedges [dg node nedges]
  "Add outgoing nedges"
  (apply add-directed-edges dg (map (fn [[dst label]]
                                      [node dst (make-label-attr label)])
                                    nedges)))

(defn -add-incedges [dg node nedges]
  "Add incoming nedges"
  (apply add-directed-edges dg (map (fn [[src label]]
                                      [src node (make-label-attr label)])
                                    nedges)))

(defn -add-selfedges [dg node labels]
  "Add self-edges for given labels"
  (apply add-directed-edges dg (map #([node node %]) labels)))

(defn -dedup-edges [dg node] "Remove duplicate edges"
  (let [edge-key (fn [dg edge] [(src edge) (dest edge) (label-of dg edge)])
        remove-dupes (fn [dg edges]
                       "Group nodes by key, remove all but the first for each key"
                       (let [edges-by-key (group-by (partial edge-key dg) edges)
                             victims (apply concat (map rest (vals edges-by-key)))]
                         (remove-edges* dg victims)))]
    (as-> dg dg
      (remove-dupes dg (in-edges dg node))
      (remove-dupes dg (out-edges dg node)))))

(defn -merge [dg adopting victim]
  "Merge victim into adopting"
  (let [in-nedges (incoming-nedges dg victim)
        out-nedges (outgoing-nedges dg victim)
        slabels (self-labels dg victim)]
    (assert (not= adopting victim) "Can't merge node into self")
    (as-> dg dg
      (remove-nodes dg victim)
      (-add-incedges dg adopting in-nedges)
      (-add-outedges dg adopting out-nedges)
      (-add-selfedges dg adopting slabels)
      (-dedup-edges dg adopting))))

(defn -nodes-same-outgoing [dg node1 node2]
  "return whether the two nodes have the same self and outgoing nedges"
  (and (= (self-labels dg node1) (self-labels dg node2))
       (= (outgoing-nedges dg node1) (outgoing-nedges dg node2))))

(defn -nodes-same-incoming [dg node1 node2]
  "return whether the two nodes have the same self and incoming nedges"
  (and (= (self-labels dg node1) (self-labels dg node2))
       (= (incoming-nedges dg node1) (incoming-nedges dg node2))))

(defn -apply-merging-opt [dg pred]
  (let [nds (nodes dg)
        worklist (neq-cartesian nds nds)
        merger (fn [dg [node1 node2]]
                 (if (and (has-node? dg node1)
                          (has-node? dg node2)
                          (pred dg node1 node2))
                   (-merge dg node1 node2)
                   dg))]
    (reduce merger dg worklist)))

(defn ndfa-optimize [g]
  "Optimize an NDFA"
  (-apply-merging-opt g (any-pred -nodes-same-outgoing -nodes-same-incoming)))
