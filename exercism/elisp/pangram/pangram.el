;;; pangram.el --- Pangram (exercism)

;;; Commentary:

;;; Code:


(defun is-pangram (s)
  (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
         (downcase-s (downcase s))
         (char-in-s (apply-partially 'seq-contains downcase-s)))
    (seq-every-p char-in-s alphabet)))

(provide 'pangram)

;;; pangram.el ends here
