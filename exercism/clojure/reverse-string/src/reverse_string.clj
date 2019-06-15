(ns reverse-string)

(defn reverse-string [s]
  "Reverse the given string `s`."
  (let
      [char-seq (seq s)
       reversed-char-seq '()]
    (clojure.string/join (reduce conj reversed-char-seq char-seq))))
