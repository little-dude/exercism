;;; anagram.el --- Anagram (exercism)

;;; Commentary:

;;; Code:

(defun anagrams-for (word candidates)
  "Return the strings from CANDIDATES that are anagrams of WORD."
  (cl-labels ((get-letters (s) (sort (string-to-vector (downcase s)) '<)))
    (let ((word-letters (get-letters word)))
      (seq-filter
       (lambda (candidate)
         (if (and
              (not (equal (downcase word) (downcase candidate)))
              (equal word-letters (get-letters candidate)))
             t nil))
       candidates))))

(provide 'anagram)
;;; anagram.el ends here
