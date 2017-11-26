(ns longbow.ndfa2re
  (:require
   [loom.graph :refer :all]
   [loom.io :refer :all]
   [loom.attr :refer :all]
   [loom.label :refer :all]
   [loom.derived :refer :all]
   [taoensso.truss :as truss :refer (have have! have?)]
   [longbow.ndfa :refer :all]
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
  RegEx (stringify [this] (concat (map stringify rs))))
(defrecord Alt [rs]
  RegEx (stringify [this] (string/join "|" (map stringify rs))))
(defrecord Kleene [r]
  RegEx (stringify [this] (concat (stringify r) "*")))

(defn -relabel-edge
  "Replaces a literal or epsilon with an instance of our RegEx proto"
  [x]
  (cond
    (char? x) (Prim. x)
    (= x epsilon) (Empty.)
    :else (throw (Exception. "Unknown label type"))))


(def test-ndfa (add-ndfa-inputs (initial-graph) '("abc", "defg", "")))
(view (relabel stringify (relabel -relabel-edge test-ndfa)))

(defn ndfa2re
  "Convert an NDFA to a RegEx"
  [ndfa])
