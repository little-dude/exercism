(ns nucleotide-count)

(def valid-nucleotides #{\A \C \G \T})

(defn count [nucleotide strand]
  {:pre [(contains? valid-nucleotides nucleotide)]}
  (reduce
   (fn [counter item]
     (if (= item nucleotide) (inc counter) counter))
   0
   strand))

(defn nucleotide-counts [strand]
  {:post [(= valid-nucleotides (set (keys %)))]}
  (merge
   (zipmap valid-nucleotides (repeat 0))
   frequencies strand))
