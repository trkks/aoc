(ns h5 (:use clojure.pprint))


(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :as math])


(defn seeds [x]
  (let [i (str/index-of x ":")]
    (map parse-long
      (-> x
        (subs (+ i 1))
        str/trim
        (str/split #" ")))))
        
(defn group-gapped' [[big-acc small-acc] x]
  (concat (if (not (empty? x))
      [big-acc, (conj small-acc x)]
      [(conj big-acc small-acc), []])))

(defn group-gapped [xs]
  (->> xs
      (reduce group-gapped' [[] []])
      ; Finalize the last concat
      (#(let [[bacc sacc] %] (conj bacc sacc)))))

(defn map-range [x y z]
  (hash-map :dest-start x :source-start y :len z))

(defn source-to-dest [acc [label & xs]]
  (let [[from _ to] (str/split label #"-" 3)]
  (assoc acc
    from (hash-map
      :dest (first (str/split to #" " 2))
      :ranges
        (map
          #(apply map-range (map parse-long (str/split % #" ")))
          xs)))))

(defn preprocess [[x & xs]]
  (hash-map
    :seeds (seeds x)
    :maps (reduce source-to-dest {} (group-gapped (drop-while empty? xs)))))

(defn location'' [ss n ds x]
    (let [offset (- x ss)]
      (when (and (<= 0 offset) (< offset n))
         ; The offset fits the range.
         (+ offset ds))))

(def location' (memoize
  (fn [srs x]
    (some 
      #(let [{ss :source-start,
              rlen :len,
              ds :dest-start} %]
        (location'' ss rlen ds x))
      srs))))

(def location (memoize
  (fn [srs x]
    ; If none of ranges have fit, the source is implicitly mapped to
    ; "itself".
    (or (location' srs x) x))))

(def get-to-bottom'' (memoize
  (fn [l ms acc]
    (let [l' (get ms l)
          acc' (location (:ranges l') acc)]
      (if (nil? l')
        acc'
        (recur (:dest l') ms acc'))))))

(def get-to-bottom' (memoize
  (fn [layer maps [x & xs] mini]
    (if (nil? x)
      mini
      ; Here is the depth-diving call.
      (let [y (get-to-bottom'' layer maps x)
            ; Only the minimum location number is eventually interesting.
            mini' (min y mini)]
      (when (< 1500000000 x)
        (println "Returned from column: [" (format "%10d" x) "] with current minimum: [" mini' "]"))
      ; Here is the recursive call starting dive again from the top.
      (recur layer maps xs mini'))))))

(defn get-to-bottom [maps seeds]
  (get-to-bottom' "seed" maps seeds Long/MAX_VALUE))

;; Transform the targets with rtf.
(defn almanac-solution [rtf {seeds :seeds, maps :maps}]
  (get-to-bottom maps (rtf seeds)))

(defn run'
  ([rtf s]
    (->> s
        str/split-lines
        preprocess
        (almanac-solution rtf)
        )))

;; Creates ranges from target's pairs and instead of whole map saves _index_
;; (a string-key).
(defn flat-range [xs]
  (->> xs
      (partition 2)
      ((juxt
         identity
         #(println "Combined range length:" (apply + (map second %)) \newline)))
      first
      (map #(range (first %) (+ (first %) (second %))))
      flatten))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run 
  ([f] (println (run' f (slurp "resources/h5.dat"))))
  ([] (run identity)))

(defn p1 [& _] (run))
(defn p2 [& _] (println "unfinished") (run flat-range))
