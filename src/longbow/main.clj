(ns longbow.main
  (:require
   [taoensso.truss :as truss :refer (have have! have?)]
   [longbow.ndfa :refer :all]
   [loom.io :refer :all])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [dg (-> (initial-graph)
               (add-ndfa-inputs '("abc", "defg", "")))]
    (view dg :alg :dot :fmt :png)))

(-main)