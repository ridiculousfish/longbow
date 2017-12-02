(ns longbow.ndfa2re
  (:require
   [ubergraph.core :refer :all]
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
    :else (throw (Exception. (str "Unknown label type " x)))))

(defn -collapse-node
  "Collapse a node in an RE-NDFA"
  [g node]
  (let []))

  

(defn ndfa2re
  "Convert an NDFA to a RegEx"
  [ndfa])
