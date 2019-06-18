(ns beer-song
    (:require [clojure.string :as string]))

(defn bottle
  [num]
  (cond
    (= num 0) "no more bottles"
    (= num 1) "1 bottle"
    :else (str num " bottles")))

(defn first-verse
  "Return the first verse for `num` bottles."
  [num]
  (let [bottle-number (bottle num)]
    (str (string/capitalize bottle-number) " of beer on the wall, " bottle-number " of beer.")))

(defn second-verse
  "Return the second verse for `num` bottles."
  [num]
  (let [first-part (cond (= num 0) "Go to the store and buy some more, "
                         (= num 1) "Take it down and pass it around, "
                         (> num 1) (str "Take one down and pass it around, "))
        bottles-left (if (= num 0) (bottle 99) (bottle (- num 1)))
        second-part (str bottles-left " of beer on the wall.")]
    (str first-part second-part)))

(defn verse
  "Returns the nth verse of the song."
  [num]
  (str (first-verse num) "\n" (second-verse num) "\n"))


(defn sing
  "Given a start and an optional end, returns all verses in this interval. If
  end is not given, the whole song from start is sung."
  ([start] (sing start 0))
  ([start end] (string/join "\n" (map verse (range start (- end 1) -1))))) ()
