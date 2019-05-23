;;; perfect-numbers.el --- perfect-numbers Exercise (exercism)

;;; Commentary:

;;; Code:

(defun factors (n)
  "Return a list of the factors of N."
  (let ((factors (list 1))
        (i 2))
    (while (<= i (/ n 2))
      (if (zerop (% n i))
          (setq factors (cons i factors)))
      (setq i (1+ i)))
    factors))

(defun is-perfect (n)
  "Return `t` is N is a perfect number, `nil` otherwise."
  (and (/= 1 n)
       (let* ((factors (factors n))
              (factors-sum (seq-reduce '+ factors 0)))
         (= n factors-sum))))

(defun perfect-numbers (n)
  "Return the perfect numbers smaller than or equal to N."
    (seq-filter 'is-perfect (number-sequence 2 n)))

(provide 'perfect-numbers)
;;; perfect-numbers.el ends here
