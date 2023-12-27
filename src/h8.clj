(ns h8 (:use clojure.pprint))


(require '[clojure.string :as str])
(require '[clojure.math :as math])

(def start-element "AAA")
(def end-element "ZZZ")

(defn node [s]
  (let [[k vs] (str/split s #"\s*=\s*")
        v (filter #(not (empty? %)) (str/split vs #"[(, )]"))]
    [k v]))

(defn parse [ss]
  (let [kvs (apply concat (map node (rest ss)))
        ys (apply hash-map kvs)
        x (seq (first ss))
        y (get ys start-element)]
    [ys x y]))

(defn navigation [c]
  (case c \R second \L first (constantly nil)))

; Loop.
(defn step' [nodes lrs' k acc]
  (if (= k end-element)
    acc
    (recur
      nodes
      (rest lrs')
      ((navigation (first lrs')) (get nodes k))
      (inc acc))))

(defn step [nodes lrs start-choice]
  (let [lrs' (rest lrs)
       k ((navigation (first lrs)) start-choice)
       acc 1]
    (step' nodes lrs' k acc)))

(defn count-steps [nodes lrs start]
  (step nodes (flatten (repeat lrs)) start))
    
(defn p1' [s] (
  ->> s
    str/split-lines
    (filter #(not (empty? %)))
    parse
    (apply count-steps)))

(defn p2' [s] (println "unimplemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run [f] (println (f (slurp "resources/h8.dat"))))

(defn p1 [& _] (run p1'))
(defn p2 [& _] (run p2'))
