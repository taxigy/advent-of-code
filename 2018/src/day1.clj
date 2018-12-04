(ns day1)
(use '[day1data :only [data]])

(defn part1 []
  (reduce + 0 data))

(def dataz (cycle data))

(defn part2 []
  (loop [x (first dataz)
         xs (rest dataz)
         found #{}]
    (if (found x)
      x
      (recur (+ x (first xs))
             (rest xs)
             (conj found x)))))
