(def jumps (vec (map read-string (re-seq #"-?\d+" (slurp "input.txt")))))

(defn update-value [value]
  (if (>= value 3)
    (dec value)
    (inc value)))

(defn calculate [init-jumps]
  (loop [jumps init-jumps
         position 0
         times-jumped 0]
    (let [current-value (get jumps position)]
      (if (nil? current-value)
        times-jumped
        (recur (assoc jumps position (update-value current-value))
               (+ position current-value)
               (inc times-jumped))))))
