(ns h9 (:use clojure.pprint))


(require '[clojure.string :as str])
(require '[clojure.math :as math])

(require '[utils])


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

(defn p1 [s]
  (->> s
       parse
       (map dive)
       (map (partial predict + last 0))
       (apply +)))

(defn p2 [s]
  (->> s
       parse
       (map dive)
       (map (partial predict - first 0))
       (apply +)))

(defn -main [& [part _]] (utils/main-builder 9 {:part1 p1 :part2 p2} part))
