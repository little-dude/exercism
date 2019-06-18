(ns series)

(defn slices [string length]
  (if (zero? length)
    [""]
    (map (partial apply str) (sliding-window string length))))

;; note that for the exercise we implemented this function, but
;; `partition` already does that
(defn sliding-window [seq length]
  (if (<= length 0)
    nil
    (loop [result ()
           remaining seq]
      (let [chunk (take length remaining)]
        (if (= (count chunk) length)
          (recur (cons chunk result) (rest remaining))
          (if (not (empty? result))
            (reverse result)))))))
