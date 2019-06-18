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

;; I initially wanted to use `reduce` but didn't know how to exit the loop.
;; The solution is to use `reduced`, as shown in this solution:
;; https://exercism.io/tracks/clojure/exercises/collatz-conjecture/solutions/a766f26a685a4f408d84631163c9941b
;;
;; (defn collatz [n]
;;   (if (< n 1)
;;     (throw (IllegalArgumentException. (str "invalid n: " n)))
;;     (reduce (fn [n i] (cond (= n 1)  (reduced i) ; reduced is used to exit
;;                             (odd? n) (inc (* n 3))
;;                             :else    (/ n 2)))
;;             n (range))))
