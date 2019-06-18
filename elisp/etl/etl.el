;;; etl.el --- etl Exercise (exercism)

;;; Commentary:

;;; Code:

(defun etl (hash-table)
  (let ((new-hash-table (make-hash-table)))
    (maphash
     (lambda (score chars)
       (if (< score 0)
           (error "Negative score")
         (mapcar
          (lambda (char)
            (unless (stringp char)
              (error "Invalid value: not a string"))
            (unless (equal (length char) 1)
              (error (format "Invalid value: expected a char, got %s" char)))
            (puthash (downcase char) score new-hash-table))
          chars)))
     hash-table)
    new-hash-table))

(provide 'etl)
;;; etl.el ends here
