(ns longbow.main
  (:require
   [taoensso.truss :as truss :refer (have have! have?)]
   [longbow.ndfa :refer :all]
   [longbow.ndfa2re :refer :all]
   [longbow.ndfa-opts :refer :all]
   [ubergraph.core :refer :all :exclude [pprint]]
   [loom.io :refer :all])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [dg (-> (initial-graph)
               (add-ndfa-inputs '("abc", "defg", "")))]
    (do
      ;(view dg :alg :dot :fmt :png)
      )))

;(-main)

(defn view-ok [g]
  (view g)
  g)

(defn str2res-opt [& args]
  "Strings to NDFA, optimize, to RE strings"
  (-> (initial-graph)
      (add-ndfa-inputs args)
      (view-ok)
      (ndfa-optimize)
      (ndfa2re)
      (stringify)))
  
(str2res-opt "ab" "xb")