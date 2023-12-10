(ns h3 (:use clojure.pprint))


(require '[clojure.repl :refer :all])
(require '[clojure.string :as str])


(defn row-nums [s]
  (re-seq #"\d+" s))

(defn part-at-row [xs s]
  (:result
    (reduce
      #(
        let [
          start (str/index-of s %2 (:lasti %1))
          len (count %2)
          end (+ start len)]
        {
          ; Add the new found part to accumulator.
          :result
            (cons { :start start :len len } (:result %1))
          ; Save last searched part end index.
          :lasti end
        })
        ; Start with an empty accumulator and index 0.
        { :result [] :lasti 0 }
      xs)))

(def moves 
  [{:dx -1 :dy -1}
   {:dx  0 :dy -1}
   {:dx  1 :dy -1}
   {:dx -1 :dy  0}
   {:dx  1 :dy  0}
   {:dx -1 :dy  1}
   {:dx  0 :dy  1}
   {:dx  1 :dy  1}])

(defn in-bounds? [w h x y]
  (and
    (<= 0 x)
    (<= 0 y)
    (<  x w)
    (<  y h)))

(defn absolute-digit-box [w h xi yi]
  (filter
    #(in-bounds? w h (:x %) (:y %))
    (map
      #(hash-map
        :x (+ xi (:dx %))
        :y (+ yi (:dy %)))
      moves)))

;; Return surrounding "box" of the part number p.
(defn part-box [p r w h]
  (let
    [s (:start p)
     e (+ s (:len p))]
  (assoc p
    :box
    (set (flatten (map #(absolute-digit-box w h % r) (range s e)))))))

(defn parts [s]
  (part-at-row (row-nums s) s))

(defn is-symbol? [c] (not (contains? #{\. \0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c)))

(defn idx-lines [x y lines]
  (nth (first (drop y lines)) x))

(defn next-to-symbol? [p lines]
  (some
    is-symbol?
    (map
      #(idx-lines (:x %) (:y %) lines)
      (:box p))))

;; Return potential part-numbers identified on each line.
(defn parts-by-line [lines] 
  (map-indexed
    #(map
      (let [
        part-box'
          (fn [xs] (part-box xs %1 (count (first lines)) (count lines)))
        add-value (fn [p]
          (assoc p
            :value
            (parse-long (subs %2 (:start p) (+ (:start p) (:len p))))))
        f (fn [xs] (add-value (part-box' xs)))
      ]
        f) (parts %2))
    lines))

(defn schema-sum [s]
  (let [lines (str/split-lines s)]
    (apply +
      (map
        #(if (next-to-symbol? % lines) (:value %) 0)
        (flatten (parts-by-line lines))))))

(defn p1' [p] (schema-sum (slurp p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return the position of the gear symbol on success
(defn next-to-gear-symbol? [p lines]
  (some
    #(if (= \* (:c %)) (dissoc % :c))
    (map
      #(let [
        [x y] [(:x %) (:y %)]
        c (idx-lines x y lines)]
        { :c c,
          :p {:x x, :y y}
          :value (:value p) })
      (:box p))))

(defn gear-ratio-sum [s]
  (let [lines (str/split-lines s)]
    ;(apply +
      (apply + (map #(apply * (map :value %)) (filter #(= 2 (count %)) (map second (group-by :p
        (filter #(not= nil %) (map
          #(next-to-gear-symbol? % lines)
          (flatten (parts-by-line lines)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn p1' ([s] (schema-sum s)))
(defn p2' ([s] (gear-ratio-sum s)))

(defn run [f] (println (f (slurp "resources/h3.dat"))))

(defn p1 [& _] (run p1'))
(defn p2 [& _] (run p2'))
