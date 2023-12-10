(ns h4 (:use clojure.pprint))


(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :as math])


(defn card-wins [x]
  (let [
    parts   (rest (str/split x #"\||:" 3))
    nums    #(set (map parse-long (str/split (str/trim %) #" +")))
    winners (nums (first parts))
    yours   (nums (second parts))
  ] (count (set/intersection winners yours))))

(defn p1' [s]
  (->> s
      str/split-lines
      (map card-wins)
      (map #(math/floor (math/pow 2 (- % 1))))
      (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tree-sum' [acc depleting-xs]
  (if (empty? depleting-xs)
    acc
    (recur (+ 1 acc) (rest depleting-xs))))

(defn tree-sum
  ([xs] tree-sum (first xs) (rest xs))
  ([x xs]
    (if (= 0 x)
      1
      (map (tree-sum' 0 xs) (take x xs)))))

(defn p2' [s]
  (->> s
      str/split-lines
      (map card-wins)
      tree-sum
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run [f] (println (f (slurp "resources/h4.dat"))))

(defn p1 [& _] (run p1'))
(defn p2 [& _] (println "unfinished") (run p2'))
