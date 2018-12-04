(ns day2)
(use '[day2data :only [data]])

(def freqdata (map (comp set vals frequencies) data))

(defn part1 []
  (let [twos (count (filter #(contains? % 2) freqdata))
        threes (count (filter #(contains? % 3) freqdata))]
    (* twos threes)))

;; https://gist.github.com/trptcolin/171634
(defn combinations [size ls]
  (filter
   #(and (not (seq? (first %))) (= size (count %)))
   (tree-seq
    #(seq? (first %))
    seq
    (map
     (fn [x]
       (if (= size 1)
         (list x)
         (map
          #(cons x %)
          (combinations (dec size) (rest (drop-while #(not (= x %)) ls))))))
     ls))))

(def combos (combinations 2 data))

(defn sdiff [^String a ^String b]
  (filter identity (map (fn [ca cb]
         (if (= ca cb)
           ca
           nil))
       a b)))

(defn sdiffs [^String a ^String b]
  (let [diff (sdiff a b)]
    (if (= (count diff) (dec (count a)) (dec (count b)))
      diff
      nil)))

(defn part2 []
  (loop [x (first combos)
         xs (rest combos)
         found #{}]
    (if (empty? xs)
      (->> found
           (filter identity)
           first
           (clojure.string/join nil))
      (recur (first xs)
             (rest xs)
             (conj found (apply sdiffs x))))))
