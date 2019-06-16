(ns spiral-matrix)

(defn up
  "Return the position after moving up from `[row col]`.
If the `upper-bound` limit has been reached, return `nil` instead."
  ([[row col] upper-bound]
   (if (not (<= row upper-bound))
     [(dec row) col]))

  ([[row col]]
   (up [row col] (dec row))))

(defn down
    "Return the position after moving down from `[row col]`.
If the `lower-bound` limit has been reached, return `nil` instead."

  ([[row col] lower-bound]
   (if (not (>= row lower-bound))
     [(inc row) col]))

  ([[row col]]
   (down [row col] (inc row))))

(defn left
    "Return the position after moving left from `[row col]`.
If the `left-bound` limit has been reached, return `nil` instead."

  ([[row col] left-bound]
   (if (not (<= col left-bound))
     [row (dec col)]))

  ([[row col]]
   (left [row col] (dec col))))

(defn right
    "Return the position after moving right from `[row col]`.
If the `right-bound` limit has been reached, return `nil` instead."
  ([[row col] right-bound]
   (if (not (>= col right-bound))
     [row (inc col)]))

  ([[row col]]
    (right [row col] (inc col))))

(def directions {:up up :down down :left left :right right})
(def next-direction {:up :right :right :down :down :left :left :up})
(def previous-direction (clojure.set/map-invert next-direction))
;; Note that we could have defined map-invert ourself very simply with:
;; (defn map-invert
;;   [m] (reduce (fn [m [k v]] (assoc m v k)) {} m))

(defn update-limits [limits direction]
  ;; If we're going :right, and update-limits is called, it means we
  ;; reached the right-most limit and just populating a
  ;; row. Therefore, we want to update the :up limit. This is the
  ;; logic that we implement with `previous-direction`.
  (let [limit-to-update (direction previous-direction)
        ;; limits for :up and :left increase, while :down and :right
        ;; decrease
        op (if (contains? #{:up :left} limit-to-update) inc dec)]
    (update limits limit-to-update op)))

(defn update-matrix [matrix [x y] value]
  "Set `value` in the position specified by `[x y]` in `matrix`."
  (let [row (nth matrix x)
        new-row (assoc row y value)]
    (assoc matrix x new-row)))

(defn can-move [limits]
  "Return whether there can be a move within `limits`."
  (or (< (:up limits) (:down limits))
      (< (:left limits) (:right limits))))

(defn build-spiral [matrix n limits from direction]
  (if (can-move limits)

    ;; try moving in the current direction
    (let [;; the function we use to move
          move (direction directions)
          ;; the limit for the direction we're moving to
          limit (direction limits)
          ;; the next position in the matrix (can be nil) if we
          ;; reached the limit
          next-position (move from limit)]

      (if (nil? next-position)
        ;; we reached the limit for the current direction, so we need
        ;; to try the next direction.
        (let [new-limits (update-limits limits direction)
              new-direction (direction next-direction)]
          (recur matrix n new-limits from new-direction))
        ;; we were able to move to a new position, so we write the
        ;; number and continue
        (let [matrix (update-matrix matrix next-position n)]
          (recur matrix (inc n) limits next-position direction))))

    ;; we can't move anymore, so we're done
    (let [last-pos ((direction directions) from)]
      (update-matrix matrix last-pos n))))

(defn spiral [n]
  (if (<= n 0)
    '()
    (let
        [matrix (into [] (repeat n (into [] (repeat n nil))))
         value 1
         limits {:up 0 :down (dec n) :left 0 :right (dec n)}
         direction :right
         position [0 -1]]
      (build-spiral matrix value limits position direction))))
