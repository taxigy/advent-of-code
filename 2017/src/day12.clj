(def input
  (->> "input.txt"
       slurp
       clojure.string/split-lines
       (map (partial re-seq #"\d+"))
       (map (partial map read-string))))

(defn intersect? [s c]
  (some s c))

(defn union [s & ss]
  (into #{} (apply clojure.set/union s ss)))

(defn empty-keys [m ks]
  (let [m' (select-keys m ks)]
    (interleave (keys m') (map empty (vals m')))))

(defn search [result x]
  (let [found (filter #(some (second %) x) result)
        [fk fs] (first found)
        rf (rest found)]
    (if-not (empty? found)
      (apply assoc result
             fk (union fs (mapcat second rf) x)
             (empty-keys result (map first rf)))
      (assoc result (count result) (into #{} x)))))

(defn calculate [input]
  (let [max-len (apply max (map count input))
        rs (reduce search {} (take (* max-len (count input))
                                   (cycle input)))
        rs' (filter (comp not-empty second) rs)]
    (count rs')))
