(ns day5
  (require [clojure.string :as s]))

(use '[day5data :only [data]])

(defn mixcase? [a b]
  (and
   a
   b
   (not= a b)
   (= (s/lower-case a) (s/lower-case b))))

(defn react [coll]
  (loop [from 0
         xs coll]
    (if (or (= (inc from) (count xs))
            (empty? xs))
      xs
      (let [[left right] (split-at from xs)
            [a b] (take 2 right)]
        (if (mixcase? a b)
          (recur (dec from)
                 (concat left (drop 2 right)))
          (recur (inc from)
                 (concat left right)))))))

(defn part1 []
  (count (react data)))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn remove-react [coll]
  (apply min (map (fn [c]
                    (count (react (remove #(= (s/lower-case c) (s/lower-case %))
                                          coll))))
                  alphabet)))
