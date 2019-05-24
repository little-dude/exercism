;;; acronym.el --- Acronym (exercism)

;;; Commentary:

;;; Code:

(require 'cl)

(defun acronym (s)
  (let ((words (split-string s "[ \f\t\n\r\v-]" t)))
    (cl-loop for word in words
             if (not (string= "" word))
             collect (substring word 0 1) into letters
             finally (return (mapconcat 'upcase letters "")))))

(provide 'acronym)
;;; acronym.el ends here
