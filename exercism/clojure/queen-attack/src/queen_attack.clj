(ns queen-attack
  (:require [clojure.string :as s]))

(defn pos->index
  "Return the tile index for the given coordinates."
  [[row col]]
  (+ (* 8 row) col))

(defn board-string
  "Return a string represented a chess board.
If `queens` is specified, they are represented on the board."
  ([]
   (board-string {}))

  ([queens]
   (let [out-of-board [0 -1]
         w-tile (pos->index (:w queens out-of-board))
         b-tile (pos->index (:b queens out-of-board))
         tiles (map #(cond
                       (= % w-tile) \W
                       (= % b-tile) \B
                       :else \_)
                    (range 0 (* 8 8)))
         rows (map (partial s/join " ") (partition 8 tiles))]
     (str (s/join "\n" rows) "\n"))))


(defn can-attack [queens]
  "Return whether the two queens are in range of each other."
  (let [[w-row w-col] (:w queens)
        [b-row b-col] (:b queens)]
    (or
     (= w-row b-row) ; queens are on the same row
     (= w-col b-col) ; queens are on the same column
     ; queens are on the same diagonal
     (=
      (Math/abs (- w-row b-row))
      (Math/abs (- w-col b-col))))))
