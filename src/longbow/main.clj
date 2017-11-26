(ns longbow.main
  (:require
   [taoensso.truss :as truss :refer (have have! have?)]
   [longbow.ndfa :refer :all]
   [longbow.ndfa2re :refer :all]
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
      (viz-graph dg)
      )))

;(-main)

;(view (add-ndfa-inputs (initial-graph) '("abc", "defg", "")))