(ns longbow.ndfa2re
  (:require
   [ubergraph.core :refer :all]
   [taoensso.truss :as truss :refer (have have! have?)]
   [longbow.ndfa :refer :all]
   [longbow.utils :refer :all]
   [loom.io :refer (view)]
   [clojure.string :as string])
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
  RegEx (stringify [this] (string/join "|" (sort (map stringify rs)))))
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

(defn -collapse-node [g node]
  "Collapse a node in an RE-NDFA. Concatenation of incoming -> selfloops -> outgoing. "
  (let [slabels (self-labels g node)
        self-res (if (empty? slabels) [] [(Kleene. (-as-alt slabels))])
        incoming-edges (filter #(not= node (src %)) (in-edges g node))
        outgoing-edges (filter #(not= node (dest %)) (out-edges g node))
        build-label (fn [inc out] (make-label-attr (-as-concat (concat
                                                                [(label-of g inc)]
                                                                self-res
                                                                [(label-of g out)]))))
        new-edges (for [inc incoming-edges
                        out outgoing-edges]
                    [(src inc) (dest out) (build-label inc out)])]
    (as-> g g
        (remove-nodes g node)
        (apply add-directed-edges g new-edges))))

(defn -just-collapse [rendfa]
  "Collapse an RE-NDFA without regard to anything. Returns the regex!"
  (let [interior-nodes (remove #{start-node goal-node} (nodes rendfa))
        collapsed (reduce -collapse-node rendfa interior-nodes)
        labels (labels-between collapsed start-node goal-node)]
    (-as-alt labels)))

(defn ndfa2re [ndfa]
  "Convert an NDFA to a RegEx"
  (let [rendfa (relabel -relabel-edge ndfa)]
    (-just-collapse rendfa)))

(stringify (ndfa2re (add-ndfa-inputs (initial-graph) '("ab" "c"))))
