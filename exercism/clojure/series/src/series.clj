(ns series)

(defn slices [string length]
  (if (zero? length)
    [""]
    (map (partial apply str) (partition length 1 string))))
