(ns longbow.utils
  (:require
   [clojure.string :as string])
  (:gen-class))

(defn len1? [coll]
  "returns true if coll has one element"
  (and (seq coll)
       (empty? (rest coll))))

(defn first-and-only [coll]
  "returns the first item of coll, asserting it has length 1"
  (assert (len1? coll) "Collection should have length 1")
  (first coll))
