(def input {:a 873
            :b 583})

(def factors {:a 16807
              :b 48271})

(def div 2147483647)

(defn next-value [value factor div]
  (mod (* value factor) div))

(defn values [init factor div]
  (lazy-seq (cons (next-value init factor div)
                  (values (next-value init factor div) factor div))))

(defn b=? [a b]
  (= (bit-and a 2r1111111111111111)
     (bit-and b 2r1111111111111111)))

(defn calculate [input]
  (let [as (values (:a input) (:a factors) div)
        bs (values (:b input) (:b factors) div)
        eas (filter #(= (mod % 4) 0) as)
        ebs (filter #(= (mod % 8) 0) bs)
        vs (map b=? eas ebs)]
    (count (filter true? (take 5000000 vs)))))
