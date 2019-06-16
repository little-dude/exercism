(ns collatz-conjecture)

(defn collatz [num]
  (if (<= num 0)
    (throw (Exception. "expected a strictly positive integer."))
    (loop [value num
           steps 0]
      (if (= value 1)
        steps
        (let [value (if (zero? (mod value 2))
                      (/ value 2)
                      (inc (* 3 value)))
              steps (inc steps)]
          (recur value steps))))))
