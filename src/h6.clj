(ns h6 (:use clojure.pprint))


(require '[clojure.string :as str])
(require '[clojure.math :as math])


(defn distance [n ms] (bigint (math/floor (* ms (- n ms)))))

(defn distances [ms] (map #(distance ms %) (range (inc ms))))

(defn winners [xs w] (filter #(> % w) xs))

(defn races [[ts ds]]
  (let [parse (fn [s] (map parse-long (rest (str/split s #"\s+"))))]
    (hash-map
      :time (parse ts)
      :distance (parse ds))))

(defn race [[ts ds]]
  (let [parse (fn [s] (parse-long (str/join (rest (str/split s #"\s+")))))]
    [[(parse ts)] [(parse ds)]]))

(defn winways [ts ds]
  (map
    #(count (winners (first %) (second %)))
    (map vector (map distances ts) ds)))

(defn p1' [s]
  (let [m (races (str/split-lines s))]
    (apply * (winways (:time m) (:distance m)))))
        
(defn p2' [s] (apply winways (race (str/split-lines s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run [f] (println (f (slurp "resources/h6.dat"))))

(defn p1 [& _] (run p1'))
(defn p2 [& _] (run p2'))
