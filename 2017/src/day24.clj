(def input (map (partial mapv read-string)
                (map (partial drop 1)
                     (re-seq #"(\d+)\/(\d+)" (slurp "input.txt")))))

(defn split [x all]
  (let [m (group-by (fn [[a b]] (or (= a x)
                                    (= b x)))
                    all)
        head (get m true)
        norm-head (mapv (fn [[a b]] (if (= x a)
                                      [a b]
                                      [b a]))
                        head)]
    {:head norm-head
     :tail (get m false)}))

(defn possible-steps
  ([m] (possible-steps '() 0 (split 0 m)))
  ([r t m] (let [{:keys [head tail]} m]
             (if (empty? head)
               r
               (map (fn [h] (possible-steps (cons (apply + t h) r)
                                            (apply + t h)
                                            (split (second h) (concat tail
                                                                      (filter #(not= h %) head)))))
                    head)))))

(defn flat [coll]
  (mapcat #(if (every? coll? %)
             (flat %)
             (list %))
          coll))

(defn take-longest [coll]
  (let [c (sort #(compare (count %2) (count %1)) coll)
        longest (take-while #(= (count %) (count (first c))) c)]
    longest))

(defn take-strongest [coll]
  (let [m (apply max (map first coll))
        strongest (filter #(= m (first %)) coll)]
    strongest))

(defn calculate [input]
  (let [steps (possible-steps input)
        fsteps (flat nested-result)
        longest (take-longest fsteps)
        strongest (take-strongest longest)]
    (first (first strongest))))
