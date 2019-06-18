(ns roman-numerals)

(def roman->decimal {\I 1
                     \V 5
                     \X 10
                     \L 50
                     \C 100
                     \D 500
                     \M 1000})

;; The exercise is actually the reverse one, convert a number to a
;; roman numeral, but I realized that only after I implemented it so
;; I'll keep it.
(defn from-roman-numerals
  [s]
  (loop [sum 0
         digits (seq s)]

    (if (empty? digits)
      sum

      (let [digit (first digits)
            digit-value (get roman->decimal digit)
            remaining (rest digits)]

        ;; check the next digit
        (if-let [next-digit (first remaining)]
          ;; there is a next digit
          (let [next-digit-value (get roman->decimal next-digit)]
            (if (> next-digit-value digit-value)
              ;; if the next digit's value is higher, then we need to
              ;; count the two digits together and add `next-digit -
              ;; digit`
              (recur (+ sum (- next-digit-value digit-value)) (rest remaining))
              ;; otherwise, we do `sum += digit` and continue normally
              (recur (+ sum digit-value) remaining)))

          ;; there is not next digit, so add `digit` and return the sum
          (+ sum digit-value))))))

(defn numerals [n]
  (loop [s ""
         n n]
    (cond
      (>= n 1000) (recur (str s "M") (- n 1000))
      (>= n 900) (recur (str s "CM") (- n 900))
      (>= n 500) (recur (str s "D") (- n 500))
      (>= n 400) (recur (str s "CD") (- n 400))
      (>= n 100) (recur (str s "C") (- n 100))
      (>= n 90) (recur (str s "XC") (- n 90))
      (>= n 50) (recur (str s "L") (- n 50))
      (>= n 40) (recur (str s "XL") (- n 40))
      (>= n 10) (recur (str s "X") (- n 10))
      (>= n 9) (recur (str s "IX") (- n 9))
      (>= n 5) (recur (str s "V") (- n 5))
      (>= n 4) (recur (str s "IV") (- n 4))
      (>= n 1) (recur (str s "I") (- n 1))
      :else s)))
