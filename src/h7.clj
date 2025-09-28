(ns h7 (:use clojure.pprint))


(require '[clojure.string :as str])
(require '[clojure.math :as math])

(require '[utils])


(def basic-card-values [ nil \2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A ])
(def jokered-card-values [ nil \J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A ])

(defn card-value-based [cvs xs]
  (apply +
    (map-indexed
      #(long (* %2 (math/pow (count cvs) %1)))
      (reverse xs))))

(defn card-strength' [cvs s]
  (card-value-based cvs (map #(.indexOf cvs %) s)))

(defn card-strength [cvs m]
  (let [hand (:hand m)]
    (card-strength' cvs hand)))

(defn strongest-frequentistest-value [cvs s]
  (let [values (set cvs)
        s' (filter values s)]
    (reduce-kv
      #(if (> %3 (:v %1))
        {:k %2, :v %3}
        %1)
      {:k nil, :v 0}
      (frequencies s'))))

(defn jokered? [cvs] (= (get cvs 1) \J))

(defn jokers [cvs s]
  (let [j (or (get (frequencies s) \J) 0)]
    (if (jokered? cvs) j 0)))

(defn hand-type' [cvs s]
  (let [{k :k, v :v} (strongest-frequentistest-value cvs s)
        snk (filter #(not= % k) s)
        j (jokers cvs snk)
        s' (if (jokered? cvs) (filter #(not= % \J) snk) snk)]
    (case v
      5 :fioak

      4 (if (= j 1)
          :fioak
          (if (and (jokered? cvs) (= k \J)) :fioak :fooak))

      3 (cond
          (= j 2)
            :fioak
          (= j 1)
            :fooak
          :else
            (if (and (jokered? cvs) (= k \J))
              (if (= :op (hand-type' cvs s')) :fioak :fooak)
              (if (= :op (hand-type' cvs s')) :fh :toak)))

      2 (cond
          (= j 3)
            :fioak
          (= j 2)
            :fooak
          (= j 1)
            (if (= :op (hand-type' cvs s')) :fh :toak)
          :else
            (if (and (jokered? cvs) (= k \J))
              (if (= :op (hand-type' cvs s')) :fooak :toak)
              (if (= :op (hand-type' cvs s')) :tp :op)))

      1 (cond
          (= j 4)
            :fioak
            (= j 3)
            :fooak
          (= j 2)
            :toak
          (= j 1)
            :op
          :else
            (if (and (jokered? cvs) (= k \J)) :op :hc))

      0 nil)))

(def hand-values [ :hc :op :tp :toak :fh :fooak :fioak ])

(def hand-type (memoize (fn [cvs x]
  (let [hand (:hand x)
        ty (.indexOf hand-values (hand-type' cvs hand))]
    ;(println "Hand" hand "is type" (get hand-values ty))
    ty
    ))))

(defn solve [cvs s]
  (->> s
      str/split-lines
      (map #(str/split % #" " 2))
      (map #(hash-map :hand (first %) :bid (second %)))
      (sort-by (partial card-strength cvs))
      (sort-by (partial hand-type cvs))
      (map :bid)
      (map parse-long)
      (map-indexed #(* (inc %1) %2))
      (apply +)))

(defn p1 [s] (solve basic-card-values s))

(defn p2 [s] (solve jokered-card-values s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& [part _]] (utils/main-builder 7 {:part1 p1 :part2 p2} part))
