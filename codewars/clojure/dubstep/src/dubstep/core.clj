(ns dubstep.core)

(defn song-decoder [song]
  (clojure.string/join " "
    (remove clojure.string/blank?
      (clojure.string/split song #"WUB"))))


;; a simpler solution was to just replace the WUB with a regexp:
;; (defn song-decoder [song]
;;   (clojure.string/trim (clojure.string/replace song #"(WUB)+" " ")))
