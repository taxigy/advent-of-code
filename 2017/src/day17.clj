(defn idxoff [i o l]
  (mod (+ i o 1) l))

(defn rot-right-array [n o]
  (loop [i 2
         r '(0 1)
         c 1]
    (if (= i n)
      r
      (recur (inc i)
             (concat (take (idxoff c o (count r)) r)
                     (list i)
                     (drop (idxoff c o (count r)) r))
             (idxoff c o (count r))))))

(defn calculate [input]
  (->> input
       (rot-right-array 2018)
       (drop-while #(not= 2017 %))
       second))

(defn get-next [c o i]
  (inc (mod (+ c o) i)))

(defn calculate2 [input times]
  (loop [i 1
         current 0
         target 0]
    (if (= i times)
      target
      (recur (inc i)
             (get-next current input i)
             (if (= (get-next current input i) 1)
               i
               target)))))
