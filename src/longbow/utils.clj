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

(defn cartesian [coll1 coll2]
  (for [x coll1 y coll2] (vector x y)))

(defn neq-cartesian [coll1 coll2]
  (for [x coll1 y coll2 :when (not= x y)] (vector x y)))

(defn all? [lst] (every? identity lst))

(defn any-pred [& preds]
  "return a composed predicate that, when invoked with 'args',
   returns the first true value of the given predicates invoked with args (lazily)"
  (let [orit (fn [predseq args]
               (if (empty? predseq)
                 false
                 (or (apply (first predseq) args)
                     (recur (rest predseq) args))))]
    (fn [& args] (orit (seq preds) args))))

(defn zipmapset [coll]
  "let coll be a sequence of vectors [key, value]. Return a map key -> set(value)"
  (into {} (for [[k v] (group-by first coll)] [k (set (map second v))])))

  (defn invert-map [m]
    "invert a map, with values mapped to sets of keys"
    (reduce (fn [m' [k v]] (update m' v clojure.set/union #{k})) {} m))
    