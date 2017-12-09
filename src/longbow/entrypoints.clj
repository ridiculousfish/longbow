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
    (-> (initial-graph)
        (add-ndfa-inputs args)
        (ndfa-optimize)
        (ndfa2re)
        (stringify)))
  