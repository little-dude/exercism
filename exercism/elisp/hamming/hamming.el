;;; hamming.el --- Hamming (exercism)

;;; Commentary:

;;; Code:

(defun hamming-distance (s1 s2)
  "Return the hamming distance between S1 and S2. The two given
sequences must have the same length, oftherwise, this function
returns an error."
  
  (when (/= (length s1) (length s2))
    throw "The two sequences don't have the same length.")
  
  (let
      ((initial-diff 0)
       (pairs (seq-mapn #'cons s1 s2))
       (f (lambda (accumulated-diff pair)
            (if (equal (car pair) (cdr pair))
                accumulated-diff
              (+ accumulated-diff 1)))))
    (seq-reduce f pairs initial-diff)))
(provide 'hamming)
;;; hamming.el ends here
