;;; armstrong-numbers.el --- armstrong-numbers Exercise (exercism)

;;; Commentary:

;;; Code:

(require 'seq)

(defun armstrong-p (n)
  "Return `t` is N is an armstrong number `nil` otherwise."
  (let ((quotient n)
        (nb-digits 0)
        (digits))
    (while (not (zerop quotient))
      (let ((remainder (% quotient 10)))
        (setq nb-digits (1+ nb-digits))
        (setq quotient (/ quotient 10))
        (setq digits (cons remainder digits))))
    (= n (seq-reduce '+ (seq-map (lambda (i) (expt i nb-digits)) digits) 0))))

(provide 'armstrong-numbers)
;;; armstrong-numbers.el ends here
