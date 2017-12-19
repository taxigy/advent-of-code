(def input
  (->> "input.txt"
       slurp
       (re-seq #"(.{3})\s(\S+)\s?(\S+)?\n")))

(defn num? [s]
  (re-matches #"-?\d+" s))

(defn in [result reg value]
  (if (empty? (get result reg))
    (assoc result reg (list value))
    (update result reg #(cons value %))))

(defn out [result reg]
  (if (empty? (get result reg))
    (assoc result reg '())
    (update result reg #(pop %))))

(defn refnum [result val]
  (let [n (read-string val)
        r (result val)
        v (or (first r) 0)]
    (if (number? n)
      n
      v)))

(defn do-snd [result reg freq]
  result)

(defn do-set [result reg freq]
  (let [v (refnum result freq)]
    (in result reg v)))

(defn do-add [result reg freq]
  (let [v (refnum result freq)
        l (first (get result reg))]
    (in result reg (+ (or l 0) v))))

(defn do-mul [result reg freq]
  (let [v (refnum result freq)
        l (first (get result reg))]
    (in result reg (* (or l 0) v))))

(defn do-mod [result reg freq]
  (let [v (refnum result freq)
        l (first (get result reg))]
    (in result reg (mod (or l 0) v))))

(defn do-rcv [result reg]
  (let [r (get result reg)
        v (or (first r) 0)]
    (if (= v 0)
      result
      (out result reg))))

(defn do-jgz? [result reg offset]
  (let [v (refnum result offset)
        l (first (get result reg))
        r (or v l 0)]
    (and (not= l 0) (not= r 0))))

(def actions {"snd" do-snd
              "set" do-set
              "add" do-add
              "mul" do-mul
              "mod" do-mod})

(defn calculate [input]
  (loop [instructions input
         i 0
         result {}]
    (let [[_ current reg freq] (first instructions)]
      (cond
        (and (= current "rcv")
             (not= (first (get result reg)) 0)) [current reg freq result]
        (and (= current "jgz")
             (do-jgz? result reg freq)) (recur (drop (+ i (refnum result freq)) input)
                                              (+ i (refnum result freq))
                                              result)
        (contains? actions current) (recur (rest instructions)
                                           (inc i)
                                           ((actions current) result reg freq))
        :else (recur (rest instructions)
                     (inc i)
                     result)))))

;; gives the right answer, but ugly
