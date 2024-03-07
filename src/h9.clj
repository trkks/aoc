(ns h9 (:use clojure.pprint))


(require '[clojure.string :as str])
(require '[clojure.math :as math])

(defn parse [s]
  (->> s
      str/split-lines
      (map #(map parse-long (str/split % #"\s")))))

(defn paired-diffs [xs]
  (map
    #(apply - (reverse %))
    (map vector xs (drop 1 xs))))

(defn dive' [acc xs]
  (if (every? #(= 0 %) xs)
    (conj acc xs)
    (dive' (conj acc xs) (paired-diffs xs))))

(defn dive [xs] (dive' () xs))

(defn predict [f g acc [x & xs]]
  (if (empty? xs)
    (f (g x) acc)
    (recur f g (f (g x) acc) xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run [f] (println (f (slurp "resources/h9.dat"))))
;(defn run [f] (println (f (slurp "resources/h9.dat.sample"))))

(defn p1' [s]
  (->> s
       parse
       (map dive)
       (map (partial predict + last 0))
       (apply +)))

(defn p2' [s]
  (->> s
       parse
       (map dive)
       (map (partial predict - first 0))
       (apply +)))

(defn p1 [& _] (run p1'))
(defn p2 [& _] (run p2'))
