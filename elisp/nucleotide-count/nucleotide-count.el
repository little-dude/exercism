;;; nucleotide-count.el --- nucleotide-count Exercise (exercism)

;;; Commentary:

;;; Code:
(defun nucleotide-count (s)
  "Return the count of nucleotides in S."
  (let ((result (list (cons ?A 0) (cons ?C 0) (cons ?G 0) (cons ?T 0))))
    (seq-reduce
     (lambda (result c)
       (let ((count (cdr (assoc c result))))
         (if (equal c nil)
             (error (format "Invalid nucleotide %c" c))
           (setf (cdr (assoc c result)) (1+ count)))
         result))
     s
     result)))
    
(provide 'nucleotide-count)
;;; nucleotide-count.el ends here
