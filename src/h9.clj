(ns h9 (:use clojure.pprint))


(require '[clojure.string :as str])
(require '[clojure.math :as math])

(defn parse [s]
  (->> s
      str/split-lines
      (map #(map parse-long (str/split % #"\s")))))

(defn dive' [prev-last xs]
  (if (every? #(= 0 %) xs)
    prev-last
    (+
      prev-last
      (dive'
        (last xs)
        (map #(apply - (reverse %)) (map vector xs (drop 1 xs)))))))

(defn dive [xs] (dive' 0 xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run [f] (println (f (slurp "resources/h9.dat"))))
;(defn run [f] (println (f (slurp "resources/h9.dat.sample"))))

(defn p1' [s]
  (->> s
       parse
       (map dive)
       (apply +)))

(defn p2' [s] (parse s))

(defn p1 [& _] (run p1'))
(defn p2 [& _] (println "unimplemented") (run p2'))
