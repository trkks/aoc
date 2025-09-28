(ns h2)


(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])

(require '[utils])


(defn max-cubes'
  ([c nc-pairs]
    (apply max
      (map
        #(utils/java-int (first %))
        (filter #(= c (second %)) nc-pairs)))))

(defn max-cubes
  ([c xs]
    (max-cubes'
      (case c
        :red "red"
        :blue "blue"
        :green "green")
      (partition 2 xs))))

(defn game-sets [s]
  (let [[id & xs] (re-seq #"[0-9]+|red|blue|green" s)]
    (hash-map
       :id (utils/java-int id),
       :max-red (max-cubes :red xs),
       :max-blue (max-cubes :blue xs),
       :max-green (max-cubes :green xs))))

(def maxes { :red 12, :blue 14, :green 13 })

(defn games [s]
  (map game-sets (str/split-lines s)))

(defn possible-games [xs]
  (filter
    #(and
     (<= (:max-red %)   (:red maxes)  )
     (<= (:max-blue %)  (:blue maxes) )
     (<= (:max-green %) (:green maxes)))
    xs))

(defn game-powers [xs]
  (map #(apply * (vals (dissoc % :id))) xs))

(defn p2' [s]
  (apply + (game-powers (games s))))

(defn p1' [s]
  (apply +
    (map
      :id
      (possible-games (map game-sets (str/split-lines s))))))

(defn run [f input] (f input))

(def p1 (partial run p1'))
(def p2 (partial run p2'))

(defn -main [& [part _]] (utils/main-builder 2 {:part1 p1 :part2 p2} part))
