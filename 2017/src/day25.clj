(def instructions {:a {0 {:write 1 :move inc :next :b}
                       1 {:write 0 :move dec :next :c}}
                   :b {0 {:write 1 :move dec :next :a}
                       1 {:write 1 :move dec :next :d}}
                   :c {0 {:write 1 :move inc :next :d}
                       1 {:write 0 :move inc :next :c}}
                   :d {0 {:write 0 :move dec :next :b}
                       1 {:write 0 :move inc :next :e}}
                   :e {0 {:write 1 :move inc :next :c}
                       1 {:write 1 :move dec :next :f}}
                   :f {0 {:write 1 :move dec :next :e}
                       1 {:write 1 :move inc :next :a}}})

(defn next-state [{:keys [state position result]} instructions]
  (let [current-value (or (get result position) 0)
        {:keys [write move next]} (get-in instructions [state current-value])]
    {:state next :position (move position) :result (assoc result position write)}))

(defn calculate [input]
  (let [initial-state {:state :a :position 0 :result {}}
        {result :result} (reduce next-state initial-state (repeat input instructions))]
    (apply + (vals result))))
