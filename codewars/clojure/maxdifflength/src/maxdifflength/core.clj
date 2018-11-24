(ns maxdifflength.core)

(defn mxdiflg [a1 a2]
  (let [diffs (for [x a1 y a2] (Math/abs (- (count x) (count y))))]
    (if (empty? diffs)
      -1
      (apply max diffs))))
