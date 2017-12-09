(ns longbow.main
  (:require
   [longbow.entrypoints :refer :all]
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (str2res-opt "ab" "xb")
)


