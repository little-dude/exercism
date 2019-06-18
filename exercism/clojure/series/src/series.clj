(ns series)

(defn slices [string length]
  (if (zero? length)
    [""]
    (map (partial apply str) (sliding-window string length))))

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
