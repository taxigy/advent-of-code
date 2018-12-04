(ns day3)
(use '[day3data :only [data]])

(def numdata
  (->> data
       (map (partial re-seq #"\d+"))
       (map (partial map read-string))))

(defn xy->canvas [x y w h]
  (for [xs (range x (+ x w))
        ys (range y (+ y h))]
    [xs ys]))

(defn claim [canvas [id x y w h]]
  (loop [ps (xy->canvas x y w h)
         canvas' canvas]
    (if (empty? ps)
      canvas'
      (recur (rest ps)
             (update canvas' (first ps) (partial cons id))))))

(defn part1 []
  (->> numdata
       (reduce claim {})
       vals
       (filter #(> (count %) 1))
       count))

(defn part2 []
  (let [claims (reduce claim {} numdata)
        ids (vals claims)
        overlapping-ids (set (flatten (filter #(> (count %) 1) ids)))
        non-overlapping-ids (filter (complement overlapping-ids) (flatten ids))]
    (first non-overlapping-ids)))
