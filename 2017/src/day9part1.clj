(defn unterminate [s] (clojure.string/replace s #"!." ""))

(defn ungarbage [s] (clojure.string/replace s #"<.*?>" ""))

(defn uncomma [s] (clojure.string/replace s #"," ""))

;; hack
(defn into-vec [s]
  (-> s
      (clojure.string/replace #"\{" "[")
      (clojure.string/replace #"\}" "]")
      read-string
      eval))

(defn scores
  ([v] (scores v 1 []))
  ([v l r] (if (empty? v)
             (if (= l 1)
               r
               l)
             (concat r
                     [l]
                     (map #(scores % (inc l) r) v)))))

(defn sum [v]
  (apply + v))

(defn calculate [input]
  (-> input
      unterminate
      ungarbage
      uncomma
      into-vec
      scores
      flatten
      sum))
