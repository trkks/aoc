(ns h4 (:use clojure.pprint))


(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :as math])

(require '[utils])

(defn card-wins [s]
  (let [
    parts   (rest (str/split s #"\||:" 3))
    nums    #(set (map parse-long (str/split (str/trim %) #" +")))
    winners (nums (first parts))
    yours   (nums (second parts))
  ] (count (set/intersection winners yours))))

(defn p1 [s]
  (->> s
      str/split-lines
      (map card-wins)
      (map #(math/floor (math/pow 2 (- % 1))))
      (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; How many copies does a card x win recursively excluding the card itself.
(def card-copiesn (memoize
  (fn [[x & xs]]
    (case x
      nil 0
      0 0
      (let [n (count (take x xs))
            m (apply + (map #(card-copiesn (drop % xs)) (range n)))
            acc (+ n m)]
        acc)))))

(defn p2 [s]
  (let [xs (map card-wins (str/split-lines s))
        n (count xs)]
    (->> xs
        repeat
        (take n)
        (map-indexed #(drop %1 %2))
        (map card-copiesn)
        (apply +)
        (+ n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& [part _]] (utils/main-builder 4 {:part1 p1 :part2 p2} part))
