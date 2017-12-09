(ns longbow.ndfa2re
  (:require
   [ubergraph.core :refer :all]
   [longbow.ndfa :refer :all]
   [longbow.utils :refer :all]
   [loom.io :refer (view)]
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo])
  (:gen-class))

(defprotocol RegEx
  "What regexs can do"
  (stringify [this] "To string"))

(defrecord Empty []
  RegEx (stringify [this] ""))

(defrecord Prim [c]
  RegEx (stringify [this] (str c)))

(defrecord Concat [rs]
  RegEx (stringify [this] (apply str (map stringify rs))))
(defrecord Alt [rs]
  RegEx (stringify [this] (str "(" (string/join "|" (sort (map stringify rs))) ")")))
(defrecord Kleene [r]
  RegEx (stringify [this] (str (stringify r) "*")))

(defn -as-concat [rs]
  "Turn REs into a Concat unless it has length 1"
  (assert (not-empty rs) "Must not be empty")
  (if (len1? rs) (first-and-only rs) (Concat. rs)))

(defn -as-alt [rs]
  "Turn REs into an Alt unless it has length 1"
  (assert (not-empty rs) "Must not be empty")
  (if (len1? rs) (first-and-only rs) (Alt. rs)))

(defn -relabel-edge  [x]
  "Replaces a literal or epsilon with an instance of our RegEx proto"
  (cond
    (char? x) (Prim. x)
    (= x epsilon) (Empty.)
    :else (throw (Exception. (str "Unknown label type " x)))))

(defn -merge-parallel-nedges [nedges]
  "Group nedges with the same node into parallel nedges. Return new nedges."
  (let [flatten (fn ([[node, nedges]]
                     [node, (-as-alt (map nedge-label nedges))]))]
    (map flatten (seq (group-by nedge-node nedges)))))

(defn -collapse-node [g node]
  "Collapse a node in an RE-NDFA. Concatenation of incoming -> selfloops -> outgoing. "
  (let [slabels (self-labels g node)
        self-res (if (empty? slabels) [] [(Kleene. (-as-alt slabels))])
        build-label (fn [inc out] (make-label-attr (-as-concat (concat
                                                                [(nedge-label inc)]
                                                                self-res
                                                                [(nedge-label out)]))))
        new-edges (for [inc (-merge-parallel-nedges (incoming-nedges g node))
                        out (-merge-parallel-nedges (outgoing-nedges g node))]
                    [(nedge-node inc) (nedge-node out) (build-label inc out)])]
    (as-> g g
      (remove-nodes g node)
      (apply add-directed-edges g new-edges))))

(defn -collapse-by-path [rendfa nodelist]
  "Collapse an RE-NDFA according to the nodes in the given nodelist"
  (reduce -collapse-node rendfa nodelist))

(defn -collapsed-label [collapsed]
  "return the final label of a collapsed rendfa"
  (-as-alt (labels-between collapsed start-node goal-node)))

(defn -just-collapse [rendfa]
  "Collapse an RE-NDFA without regard to anything. Returns the regex!"
  (let [interior-nodes (remove #{start-node goal-node} (nodes rendfa))]
    (-collapsed-label (-collapse-by-path rendfa interior-nodes))))

(defn -collapsed-ndfas-every-which-way [rendfa]
  "Return a sequence of all collapsed NDFAs"
  (let [interior-nodes (remove #{start-node goal-node} (nodes rendfa))
        collapse-then-label (comp -collapsed-label (partial -collapse-by-path rendfa))]
    (map collapse-then-label (combo/permutations interior-nodes))))

(defn ndfa2re [ndfa]
  "Convert an NDFA to a RegEx"
  (let [rendfa (relabel -relabel-edge ndfa)]
    (-just-collapse rendfa)))

(defn ndfa2res [ndfa]
  "Convert an NDFA to many RegExs"
  (let [rendfa (relabel -relabel-edge ndfa)]
    (-collapsed-ndfas-every-which-way rendfa)))

(defn strs2res [& args]
  (-> (initial-graph)
      (add-ndfa-inputs args)
      (ndfa2re)
      (stringify)))

