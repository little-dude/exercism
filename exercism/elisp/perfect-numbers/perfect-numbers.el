;;; perfect-numbers.el --- perfect-numbers Exercise (exercism)

;;; Commentary:

;;; Code:

(defun factors-of (n)
  "Return a list of the factors of N."
  (let ((factors nil))
    (dotimes (i (+ 2 (/ n 2)) factors)
      (when (and (not (zerop i)) (zerop (% n i)))
          (setq factors (cons i factors))))))


(defun is-perfect (n)
  "Return `t` is N is a perfect number, `nil` otherwise."
  (and (/= 1 n)
       (let* ((factors (factors-of n))
              (factors-sum (seq-reduce '+ factors 0)))
         (= n factors-sum))))

(defun perfect-numbers (n)
  "Return the perfect numbers smaller than or equal to N."
  (let* ((perfect-numbers '(6))
         (highest (car (last perfect-numbers)))
         (new-perfect-numbers (seq-filter 'is-perfect (number-sequence (1+ highest) n))))
    (nconc perfect-numbers new-perfect-numbers)
    (seq-filter (apply-partially '>= n) perfect-numbers)))

(provide 'perfect-numbers)
;;; perfect-numbers.el ends here
