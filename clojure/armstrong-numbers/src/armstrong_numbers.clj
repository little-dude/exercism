(ns armstrong-numbers)

(defn pow [n exp]
  "Returns n^exp"
  (if (zero? exp)
    1
    (reduce * (repeat exp n))))

(defn to-digits
  "Returns the sequence of digits that compose a number, in reverse order"
  ([n]
   (to-digits (list) n))
  ([digits n]
   (if (< n 10)
    (cons n digits)
    (cons (mod n 10) (to-digits (quot n 10))))))

(defn armstrong? [n]
  "Returns whether the given number is an armstrong number"
  (def digits (to-digits n))
  (= n (reduce + (map #(pow % (count digits)) digits))))
