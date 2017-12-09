(ns longbow.entrypoints
  (:require
   [longbow.ndfa :refer :all]
   [longbow.ndfa2re :refer :all]
   [longbow.ndfa-opts :refer :all]
   [ubergraph.core :refer :all :exclude [pprint]]
   [loom.io :refer :all])
  (:gen-class))

(defn str2res-opt [& args]
  "Strings to NDFA, optimize, to RE strings"
  (let [maybe-view (if (some #{:view :show} args) view identity)
        stringify-all (partial map stringify)
        shortest (partial apply min-key count)
        ]
    (-> (initial-graph)
        (add-ndfa-inputs (filter string? args))
        (ndfa-optimize)
        (maybe-view)
        (ndfa2res)
        (stringify-all)
        (shortest))))
