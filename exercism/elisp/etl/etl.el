;;; etl.el --- etl Exercise (exercism)

;;; Commentary:

;;; Code:

(defun etl (hash-table)
  (let ((new-hash-table (make-hash-table)))
    (maphash
     (lambda (score letters)
       (if (< score 0)
           (error "Negative score")
         (cl-loop for letter in letters
                  do (progn
                       (unless (stringp letter)
                         (error "Invalid value: not a string"))
                       (unless (equal (length letter) 1)
                         (error (format "Invalid value: expected a letter, got %s" letter)))
                       (puthash (downcase letter) score new-hash-table)))))
     hash-table)
    new-hash-table))

(provide 'etl)
;;; etl.el ends here
