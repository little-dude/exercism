(ns nucleotide-count)

;; count exists of course, but part of the exercise to re-implement it
(defn count [nucleotide strand]
  (if (not (contains? #{\A \C \G \T} nucleotide))
    (throw (Exception. "invalid nucleotide"))
    (reduce (fn [counter item] (if (= item nucleotide) (inc counter) counter))
            0
            strand)))

(defn nucleotide-counts [strand]
  (reduce (fn [counts nucleotide]
            (if (contains? counts nucleotide)
              ;; I initially used `assoc`, but `update` is a better fit
              ;; (assoc counts nucleotide (inc nucleotide counts))
              (update counts nucleotide inc)
              (throw (Exception. "invalid nucleotide"))))
          {\A 0 \C 0 \T 0 \G 0}
          strand))
