;;; two-fer.el --- Two-fer Exercise (exercism)

;;; Commentary:

;;; Code:

(defun two-fer (&optional name)
  "Return \"One for NAME, one for me\". if NAME is not
provided, it defaults to \"you\"."
  (format "One for %s, one for me." (or name "you")))

(provide 'two-fer)
;;; two-fer.el ends here
