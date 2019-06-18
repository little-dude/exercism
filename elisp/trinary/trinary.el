;;; trinary.el --- Trinary (exercism)

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun trinary-to-decimal (s)
  "Return the integer corresponding to the ternary number S."
  (catch 'invalid-ternary
    (seq-reduce
     (lambda (sum i) (+ sum (* (car i) (expt 3 (cdr i)))))
     (seq-map-indexed
      'cons
      (reverse
       (seq-map
        (lambda (c)
          (let ((digit (- c ?0)))
            (if (not (and (< digit 3) (>= digit 0)))
                (throw 'invalid-ternary 0)
              digit)))
        s)))
     0)))

(provide 'trinary)
;;; trinary.el ends here
