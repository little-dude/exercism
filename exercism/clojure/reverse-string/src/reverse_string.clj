(ns reverse-string)

(defn reverse-string [s]
  "Reverse the given string `s`."
  ; this relis on the fact that for lists,
  ;
  ;      `(conj my-list my-scalar)`
  ;
  ; prepends `my-scalar` to `my-list`. So a solution could be:
  ;
  ;      `(apply conj nil (seq s))`
  ;
  ; But `(apply conj) is basically equivalent to `into`.
  ; See: https://www.braveclojure.com/core-functions-in-depth/#The_Collection_Abstraction
  (clojure.string/join (into nil (seq s))))
