(defn rotate-right [v n]
  (let [q (- (count v) n)]
    (vec (concat (drop q v)
                 (take q v)))))

(defn swap [v i j]
  (assoc v j (v i) i (v j)))

(defn swap-vals [v x y]
  (let [i (.indexOf v x)
        j (.indexOf v y)]
    (swap v i j)))

(def input
  (-> "input.txt"
      slurp
      (clojure.string/split #",")))

(defn parse-num [s]
  (->> s
       rest
       clojure.string/join
       read-string))

(defn parse-nums [s]
  (->> s
       rest
       clojure.string/join
       (re-seq #"\d+")
       (map read-string)))

(defn parse-vals [s]
  (->> s
       rest
       clojure.string/join
       (re-seq #"[a-p]")
       (map first)))

(defn transform [v op]
  (case (first op)
    \s (rotate-right v (parse-num op))
    \x (apply swap v (parse-nums op))
    \p (apply swap-vals v (parse-vals op))
    :default v))

(defn dance [input init]
  (loop [ops input
         result (vec init)]
    (if (empty? ops)
      (clojure.string/join result)
      (recur (rest ops)
             (transform result (first ops)))))))

(defn dances [input init]
  (lazy-seq (cons (dance input init)
                  (dances input (dance input init)))))

(defn detect-loop [coll]
  (loop [[x & xs] coll
         mem {}
         step 0]
    (if (mem x)
      {:cycle (- step (mem x))
       :offset (mem x)}
      (recur xs
             (assoc mem x step)
             (inc step)))))

(defn calculate [input]
  (let [ds (dances input "abcdefghijklmnop")
        {cyc :cycle off :offset} (detect-loop ds)
        rep (mod 1000000000 cyc)
        result (nth ds (+ off rep -1))]
    result))
