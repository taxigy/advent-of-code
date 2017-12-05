(def turns {:right :up
            :up :left
            :left :down
            :down :right})

(defn get-next-position [direction [i j]]
  (cond
    (= direction :right) [i (inc j)]
    (= direction :up) [(dec i) j]
    (= direction :left) [i (dec j)]
    (= direction :down) [(inc i) j]))

(defn get-steps-after-turn [direction steps-initial]
  (cond
    (= direction :up) (inc steps-initial)
    (= direction :down) (inc steps-initial)
    :otherwise steps-initial))

(defn get-move [{direction :direction
                 steps-initial :steps-initial
                 steps-left :steps-left
                 position :position}]
  (cond
    (= steps-left 0) {:direction (turns direction)
                      :steps-initial (get-steps-after-turn direction steps-initial)
                      :steps-left (get-steps-after-turn direction steps-initial)
                      :position (get-next-position (turns direction) position)}
    :otherwise {:direction direction
                :steps-initial steps-initial
                :steps-left (dec steps-left)
                :position (get-next-position direction position)}))

(defn square-indices [[i j]]
  (for [x [(dec i) i (inc i)]
        y [(dec j) j (inc j)]]
       (vector x y)))

(defn val0 [spiral [i j]]
  (let [e (spiral [i j])]
    (if (isa? e nil)
      0
      e)))

(defn calculate-square [spiral [i j]]
  (apply + (map #(val0 spiral %) (square-indices [i j]))))

(defn lazy-spiral
  ([] (lazy-spiral {[0 0] 1} {:direction :right :steps-initial 0 :steps-left 1 :position [0 0]}))
  ([spiral movement] (lazy-seq (cons (calculate-square spiral (:position movement))
                                     (lazy-spiral (assoc spiral (:position movement) (calculate-square spiral (:position movement)))
                                                  (get-move movement))))))
