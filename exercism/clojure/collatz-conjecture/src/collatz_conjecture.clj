(ns collatz-conjecture)

(defn collatz [num]
  (if (<= num 0)
    (throw (IllegalArgumentException. "expected a strictly positive integer"))
    (loop [value num
           steps 0]
      (cond
        (= value 1) steps
        (even? value)  (recur (/ value 2) (inc steps))
        :else (recur (inc (* 3 value)) (inc steps))))))
