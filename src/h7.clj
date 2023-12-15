(ns h7 (:use clojure.pprint))


(require '[clojure.string :as str])
(require '[clojure.math :as math])


(def card-values [ nil \2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A ])

(defn card-value-based [xs]
  (apply +
    (map-indexed
      #(long (* %2 (math/pow (count card-values) %1)))
      (reverse xs))))

(defn card-strength' [s]
  (card-value-based (map #(.indexOf card-values %) s)))

(defn card-strength [m]
  (let [hand (:hand m)]
    (card-strength' hand)))

(def hand-values [ :hc :op :tp :toak :fh :fooak :fioak ])

(defn frequentistest-value [s]
  (let [values (set card-values)
        s' (filter values s)]
    (reduce-kv
      #(if (and (some? %1) (> %3 (:v %1))) {:k %2 :v %3} %1)
      {:v -1}
      (frequencies s'))))

(defn noak [n s]
  (let [{k :k, v :v} (frequentistest-value s)]
    (if (>= v n) k nil)))

(defn noak? [n s] (some? (noak n s)))

(defn hand-type' [s]
  (let [{k :k, v :v} (frequentistest-value s)
        s' (filter #(not= k %) s)]
    (case v
      5 :fioak
      4 :fooak
      3 (if (= :op (hand-type' s')) :fh :toak)
      2 (if (= :op (hand-type' s')) :tp :op)
      1 :hc
      -1 nil)))

(defn hand-type [x]
  (let [hand (:hand x)]
    (.indexOf hand-values (hand-type' hand))))

(defn p1' [s]
  (->> s
      str/split-lines
      (map #(str/split % #" " 2))
      (map #(hash-map :hand (first %) :bid (second %)))
      (sort-by card-strength)
      (sort-by hand-type)
      (map :bid)
      (map parse-long)
      (map-indexed #(* (inc %1) %2))
      (apply +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn p2' [s] (println "unimplemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run [f] (println (f (slurp "resources/h7.dat"))))

(defn p1 [& _] (run p1'))
(defn p2 [& _] (run p2'))
