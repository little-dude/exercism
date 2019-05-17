;;; crypto-square.el --- Crypto Square (exercism)

;;; Commentary:

;;; Code:


;; this is needed because we use generators (see `iter-rectangle-sizes` below).
(setq lexical-binding t)

(defun normalize-string (s)
  "Remove spaces and ponctuation from S, and downcase it."
  (replace-regexp-in-string "[^a-z0-9]" "" (downcase s)))

(iter-defun iter-rectangle-sizes ()
  "A generator that return rectangles with increasing sizes.
The rectangles `(r c)` are such that `c >= r` and `c - r <= 1`."
  (let ((rect '(0 0)))
    (while t
      (let ((r (car rect))
            (c (nth 1 rect)))
        (if (> c r)
            (setq rect (list (1+ r) c))
          (setq rect (list r (1+ c))))
        (iter-yield rect)))))

(defun find-fitting-rectangle (s)
  "Return a rectangle big enough to fit the string S.
The rectangle is a pair `(r c)` where `c >= r` and `c - r <= 1`."
  (let ((rectangles (iter-rectangle-sizes)))
    (cl-loop
     (let* ((rect (iter-next rectangles))
            (r (car rect))
            (c (nth 1 rect)))
       (when (<= (length s) (* r c))
         (cl-return rect))))))

(defun get-chunks (s)
  "Split the string S in `r` chunks of length `c`.
`r` and `c` are integers such that `c >= r` and `c - r <= 1`."
  (let ((r (nth 1 (find-fitting-rectangle s)))
        (s-length (length s)))
    (cl-loop for i from 0 below s-length by r
             collect (seq-subseq s i (min s-length (+ i r))))))

(defun encode-chunks (chunks)
  "Encode a list CHUNKS of `r` chunks of length `c`.
The output is a list of `c` chunks or length `r`."
  (let ((c (length chunks))
        (r (length (car chunks))))
    (cl-loop for i from 0 below r
             collect (cl-loop for j from 0 below c
                              collect (condition-case nil
                                          (elt (nth j chunks) i)
                                        (error nil))
                              into letters
                              finally return (concat (seq-filter 'identity letters))))))

(defun pad-chunks (chunks)
  "Add extra spaces to the smaller chunks."
  (let* ((chunks-length (length (car chunks)))
         (format-string (format "%%-%ds" chunks-length)))
    (seq-map
     (lambda (chunk) (setf chunk (format format-string chunk)))
     chunks)))

(defun encipher (s)
  "Encipher S."
  (string-join
   (pad-chunks
    (encode-chunks
     (get-chunks
      (normalize-string s))))
   " "))

(provide 'crypto-square)
;;; crypto-square.el ends here
