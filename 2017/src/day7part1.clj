(def file (slurp "input.txt"))

(def input (map #(re-seq #"[a-z]+" %) (clojure.string/split file #"\n")))

(defn calculate [input]
  (->> input
      (apply concat)
      frequencies
      (filter (fn [[k v]] (= v 1)))))
