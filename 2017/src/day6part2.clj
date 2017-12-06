(defn first-index-of-largest [input]
  (loop [s input
         i 0
         c (first s)
         m 0]
    (if (empty? s)
      m
      (recur (rest s)
             (inc i)
             (if (< c (first s))
               (first s)
               c)
             (if (< c (first s))
               i
               m)))))

(defn redistribute [from all]
  (let [l (count all)
        vall (vec all)
        value (get vall from)
        t (concat (repeat (inc from) 0) (repeat value 1) (repeat l 0))
        p (partition l t)
        r (apply map + p)
        s (map + r (assoc vall from 0))]
    s))

(defn calculate [input]
  (loop [all input
         done {}
         steps-before 0
         steps-after 0]
    (if (= (done all) 3)
      steps-after
      (let [i (first-index-of-largest all)
            u (redistribute i all)
            t (done u 0)]
        (recur u
               (assoc done u (inc t))
               (if (= t 0) (inc steps-before) steps-before)
               (if (= t 1) (inc steps-after) steps-after))))))
