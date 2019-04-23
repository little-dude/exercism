;;; anagram.el --- Anagram (exercism)

;;; Commentary:

;;; Code:

(require 'cl)


(defun anagrams-for (word candidates)
  "Return the strings from CANDIDATES that are anagrams of WORD."
  (let* ((get-letters (lambda (s) (sort (string-to-vector (downcase s)) '<)))
         (anagram-letters (funcall get-letters word)))

    (seq-filter
     (lambda (candidate)
       (let ((candidate-letters (funcall get-letters candidate)))
         (if (and
              (not (equal (downcase word) (downcase candidate)))
              (equal anagram-letters candidate-letters))
             t nil)))
     candidates)))

(provide 'anagram)
;;; anagram.el ends here
