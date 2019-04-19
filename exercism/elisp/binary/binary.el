;;; binary.el --- Binary exercise (exercism)

;;; Commentary:

;;; Code:


(defun valid-binary-string? (s)
  "Return `t` if the given string S contains only \"0\" and \"1\"."
  (let* ((zero-or-one? (lambda (i) (or (= ?0 i) (= ?1 i)))))
    (not (seq-some (lambda (i) (not (funcall zero-or-one? i))) s))))

(defun to-decimal (s)
  "If S is a valid binary string, return the corresponding number.
Otherwise, return 0."

  (catch 'invalid-number
    (unless (valid-binary-string? s)
      (throw 'invalid-number 0))
    
    (let* (
           ;; The least significant digits are on the right.
           (reversed (seq-reverse s))
           ;; `char-to-int` converts a character representing an
           ;; integer into that integer.
           (char-to-int (lambda (char) (- char ?0)))
           ;; Convert the sequence of characters into a sequence of
           ;; integers.
           (digit-seq (seq-map char-to-int reversed))
           ;; Decompose the sequence of numbers in base 2. This new
           ;; sequence are the terms to sum to get the result.
           (terms (seq-map-indexed (lambda (elt idx) (* elt (expt 2 idx))) digit-seq)))
      (seq-reduce '+ terms 0))))

(provide 'binary)
;;; binary.el ends here
