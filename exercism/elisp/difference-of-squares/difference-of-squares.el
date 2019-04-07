;;; difference-of-squares.el --- Difference of Squares (exercism)

;;; Commentary:

;;; Code:

(defun square-of-sum (n)
  "Return the square of (1 + 2 + ... + N)."
  (expt (apply '+ (number-sequence 1 n 1)) 2))

(defun sum-of-squares (n)
  "Return the sum of (1^2 + 2^2 + ... + N^2."
  (apply '+ (mapcar (lambda (i) (expt i 2)) (number-sequence 1 n 1))))

(defun difference (n)
  (- (square-of-sum n) (sum-of-squares n)))

(provide 'difference-of-squares)
;;; difference-of-squares.el ends here
