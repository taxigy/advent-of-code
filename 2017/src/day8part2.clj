(def input (map #(re-seq #"\S+" %) (clojure.string/split (slurp "input.txt" #"\n"))))

(def operations {"inc" +
                 "dec" -})

(def operators {">" >
                ">=" >=
                "==" =
                "!=" not=
                "<" <
                "<=" <=})

(defn safe0 [k x] (if (nil? (k x)) 0 (k x)))

(defn update-registers [registers instruction]
  (let [[target-register target-operation target-operand _ condition-register condition-operator condition-compare-to-value] instruction
        target (registers target-register)
        compared (registers condition-register)
        target-current (safe0 :current target)
        target-max (safe0 :max target)
        comparator (operators condition-operator)
        compared-current (safe0 :current compared)
        compared-value (read-string condition-compare-to-value)
        addend (read-string target-operand)
        target-upd ((operations target-operation) target-current addend)]
    (if (comparator compared-current compared-value)
      (assoc registers target-register {:current target-upd
                                        :max (max target-upd target-max)})
      registers)))

(defn calculate [input]
  (let [result (reduce #(update-registers %1 %2) {} input)
        max-value (apply max (map :max (vals result)))]
    max-value))
