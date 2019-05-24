;;; crypto-square.el --- Crypto Square (exercism) -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'generator) ; for iter-defun
(require 'subr-x)    ; for string-join, although we could just use
                     ; mapconcat
(require 'seq)       ; for the seq- functions

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
  (cl-loop for rect iter-by (iter-rectangle-sizes)
         when (<= (length s) (apply #'* rect))
         return rect))

(defun get-chunks (s)
  "Split the string S in `c` chunks of length `r`.
`r` and `c` are integers such that `c >= r` and `c - r <= 1`."
  (let ((r (nth 1 (find-fitting-rectangle s))))
    (seq-partition s r)))

(defun encode-chunks (chunks)
  "Encode a list CHUNKS of `r` chunks of length `c`.
The output is a list of `c` chunks or length `r`."
  (if chunks
      (apply (apply-partially #'cl-mapcar #'string) (pad-chunks chunks))))

(defun pad-chunks (chunks)
  "Add extra spaces to the smaller chunks."
  (let* ((chunks-length (length (car chunks))))
    (seq-map
     (lambda (chunk)
       (let ((padding (make-string (- chunks-length (length chunk)) ?\s)))
         (setf chunk (concat chunk padding))))
     chunks)))

(defun encipher (s)
  "Encipher S."
  (string-join
   (encode-chunks
    (get-chunks
     (normalize-string s)))
   " "))

(provide 'crypto-square)
;;; crypto-square.el ends here
