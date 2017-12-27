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
               (map #(possible-steps (cons (apply + t %) r)
                                     (apply + t %)
                                     (split (second %) tail))
                    head)))))

(defn calculate [input]
  (apply max (flatten (possible-steps input))))
