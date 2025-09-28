(ns h1)


(require '[clojure.repl :refer :all])

(require '[clojure.string :as str])

(require '[utils])


;(doc nil?)
;(str/upper-case "foobar")

;(apropos "file")

;(find-doc "file")

;(require '[clojure.edn :as edn])

(defn is-digit? [x] (apply <= (map int [\0 x \9])))

(defn first-digit [xs] 
  (first (drop-while #(not (is-digit? %)) xs)))

(defn match-digits [x]
  (re-seq
    #"(?=([0-9]|zero|one|two|three|four|five|six|seven|eight|nine))"
    (str/lower-case x)))


(defn digits [x]
  (map
    #(case %
      ("zero"  "0")   \0
      ("one"   "1")   \1
      ("two"   "2")   \2
      ("three" "3")   \3
      ("four"  "4")   \4
      ("five"  "5")   \5
      ("six"   "6")   \6
      ("seven" "7")   \7
      ("eight" "8")   \8
      ("nine"  "9")   \9)
    (map
      ; Take the second item because of regex-magic-reasons % is ["" "match"].
      #(first (rest %))
      (match-digits x))))


(defn pair [xs]
  (apply str (str/join [(first (digits xs)) (last (digits xs))])))

(defn pair-p1 [xs]
  (apply str (str/join [(first-digit xs) (first-digit (reverse xs))])))

(defn run [f input]
  (apply +
    (map
      #(if (str/blank? %) 0 (utils/java-int %))
      (map f (str/split-lines input)))))

(def p1 (partial run pair-p1))
(def p2 (partial run pair))

; MAIN.
(defn -main [& [part _]] (utils/main-builder 1 {:part1 p1 :part2 p2} part))
