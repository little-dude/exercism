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










;; https://exercism.io/tracks/clojure/exercises/spiral-matrix/solutions/e3dce245c5414fd29cf7863d1d2b7fd8
;;
;; (defn make-row [x y] (vector (map #(+ y %) (range x))))
;; (defn pare-down-coll [coll] (apply map vector coll))
;; (defn reverses [coll] (map reverse coll))
;; (defn glom-it-on [x y coll] (concat (make-row x y) coll))
;; (defn spiral
;;   ([n] (spiral n n 1))
;;   ([m n & [s]]
;;     (cond (= 0 n) []
;;           (= 1 n) (make-row m s)
;;           :else (->> (spiral (dec n) m (+ s m))
;;                       pare-down-coll
;;                       reverses
;;                       (glom-it-on m s)))))
;;
;;         m n &[s]
;; (spiral 4 4 1)     ((1 2 3 4) (12 13 14 5) (11 16 15 6) (10 9 8 7))
;; (spiral 3 4 5)     ((5 6 7) (14 15 8) (13 16 9) (12 11 10))
;; (spiral 3 3 8)     ((8 9 10) (15 16 11) (14 13 12))
;; (spiral 2 3 11)    ((11 12) (16 13) (15 14))
;; (spiral 2 2 13)    ((13 14) (16 15))
;; (spiral 1 2 15)    ((15) (16))
;; (spiral 1 1 16)    [(16)]




;; https://www.reddit.com/r/dailyprogrammer/comments/6i60lr/20170619_challenge_320_easy_spiral_ascension/dj50ltl/?st=jwzdtq5m&sh=05b9c3ac
;;
;; (defn rot90
;;   "Rotate rows 90 degrees clockwise."
;;   [rows]
;;   (mapv reverse (apply map list rows)))

;; (defn add-row
;;   "Adds new row, values starting at bottom left value + 1."
;;   [rows]
;;   (let [n (-> rows last first dec)
;;         width (-> rows first count)
;;         row (take width (iterate dec n))]
;;     (conj rows row)))

;; (defn solve
;;   [n]
;;   (let [seed [[(* n n)]]
;;         idx (* 2 (dec n))
;;         step (comp add-row rot90)]
;;     (-> (iterate step seed) (nth idx) rot90 rot90)))


;; from
;; https://exercism.io/tracks/clojure/exercises/spiral-matrix/solutions/f33cb550fff74c49bb0c37b1b74d343a

;; (ns spiral-matrix)
;;
;; (defn positions [{xs :xs ys :ys n :n}]
;;   (for [x xs y ys] (+ x (* y n))))
;;
;; (defn right [n d]
;;   (positions {:xs (range d (- n d))
;;               :ys [d]
;;               :n n}))
;;
;; (defn down [n d]
;;   (positions {:xs [(- (dec n) d)]
;;               :ys (range (inc d) (- n d))
;;               :n n}))
;;
;; (defn left [n d]
;;   (positions {:xs (range (- n d 2) (dec d) -1)
;;               :ys [(- (dec n) d)]
;;               :n n}))
;;
;; (defn top [n d]
;;   (positions {:xs [d]
;;               :ys (range (- n d 2) d -1)
;;               :n n}))
;;
;; (defn ring [n depth]
;;   (mapcat #(% n depth) [right down left top]))
;;
;; ;; roughly: list the "coordinates" in spiral order,
;; ;; pair each with its position in the list,
;; ;; then sort & return the re-ordered list positions
;; (defn spiral [n]
;;   (as-> (range 0 n) t
;;     (mapcat #(ring n %) t)
;;     (map vector t (iterate inc 1))
;;     (into (sorted-map) t)
;;     (vals t)
;;     (partition n t)))



;; (ns ^{:doc "Solution to exercism.io problem 'spiral-matrix'."
;;       :author "Jon Bristow"}
;;  spiral-matrix)
;;
;; (defn spiral
;;   "Returns a list of lists representing a spiral starting from 1 on the outside
;; and ending with n*n close to the middle."
;;   [n]
;;   (if-not
;;    (pos? n) '()
;; ;; I only remember this from doing too many Project Euler questions.
;; ;; Generating from the outside-in isn't much different, just map backwards.
;; ;; In project euler it becomes more important not to blow the stack, otherwise
;; ;; the recursive solution is usually fine.
;;    (->> (range (dec n) 0 -1) ; from n-1 to zero...
;;                              ; '(2 1)
;;         (mapcat (fn [x] (repeat 2 x)))
;;                              ; repeat every item in the range twice
;;                              ; '(2 2 1 1)
;;         (cons n)             ; add n to the front (n+2*sum(n-1))==(n*n)
;;                              ; '(3 2 2 1 1)
;;         (mapcat (fn [c x] (repeat x c)) (cycle [1 n -1 (- n)]))
;;                              ; Repeat cycle(1 n -1 -n) item times
;;                              ; '(1 1 1 3 3 -1 -1 -3 1)
;;         (reductions +)       ; sum every item, but keep every step
;;                              ; '(1 2 3 6 9 8 7 4 5)
;;         (map-indexed vector) ; map every item with its index
;;                              ; '([0 1] [1 2] [2 3] [3 6] .. [8 5])
;;         (sort-by second)     ; order by sum value
;;                              ; '([0 1] [1 2] [2 3] [7 4] .. [4 9])
;;         (map first)          ; only keep the first value
;;                              ; '(0 1 2 7 8 3 6 5 4)
;;         (map inc)            ; increment to match test cases
;;                              ; '(1 2 3 8 9 4 7 6 5)
;;         (partition n))))     ; split into proper rows
;;                              ; '('(1 2 3) '(8 9 4) '(7 6 5))
;;
;; ;; To elaborate a little, imagine that you are walking around the outside of
;; ;; the spiral.
;; ;;
;; ;; You start out walking left to right, and you mark a 1 n times.
;; ;; You turn right, and walk top to bottom, marking n (n-1) times.
;; ;; You turn right again, and walk right to left, marking -1 (n-1) times.
;; ;; You turn right again, and walk up, marking -n (n-2) times.
;; ;;
;; ;; And so on...
;; ;;
;; ;; Now, lets think of it as mapping one number line to another.
;; ;;
;; ;; You walk left to right, putting down 1->1, 2->2, .. n->n.
;; ;; You stop and walk to the bottom, noting: n+1->2n, n+2->3n, .. 2n-1->n*n
;; ;; Then you walk to the left, noting: 2n->n^2-1, 2n+1->n^2-2, .. 3n-2->n^2-n
;; ;; Then you walk up, noting: 3n-1->n^2-2n, 3n->n^2-2n, ..  4n-4->n
;; ;;
;; ;; I'm not sure if it's apparent, but each right hand number can be written as
;; ;; the sum of the right hand numbers before it plus some pattern. Additionally,
;; ;; they map 1:1 with our intended series.
;; ;;
;; ;; With that knowledge in hand, we can then do our simple mapping trick as
;; ;; demonstrated by the code.
