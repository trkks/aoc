(ns utils)


(defn java-int [x] (Integer. x))

(defn main-builder [hatch-num {part1-solution :part1, part2-solution :part2} selected-part]
  (let
    [ input-path (str "data/h" hatch-num ".dat")
      input (try
        (slurp input-path)
        (catch Exception e
          (throw
            (Exception.
              (str "Failed to load input: " (.getMessage e))))))
      part (if (nil? selected-part)
        :both
        (last (re-matches #"[part]*[12]{1}" selected-part)))
    ]
    (println
      (case part
        \1 (part1-solution input)
        \2 (part2-solution input)
        :both (str (part1-solution input) \newline (part2-solution input))
        (throw
          (Exception.
            (str
              "Need argument '[part]1' or '[part]2': Got "
              (if (empty? part)
                "no arguments"
                (str "'" part "'"))
              ".")))))))
