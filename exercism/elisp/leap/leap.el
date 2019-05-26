;;; leap.el --- Leap exercise (exercism)

;;; Commentary:

;;; Code:

(defun leap-year-p (year)
  (if (zerop (% year 4))
      (if (zerop (% year 100))
          (if (zerop (% year 400))
              t ; divisible by 4, 100 and 400
            nil ; divisible by 4, 100 but not 400
            )
        t ; divisible by 4 but not 100
        )
      nil ; not divisible by 4
      ))

(provide 'leap)
;;; leap.el ends here
