;;; allergies.el --- Allergies Exercise (exercism)

;;; Commentary:

;;; Code:

(setq allergens
'("eggs" "peanuts" "shellfish" "strawberries" "tomatoes" "chocolate" "pollen" "cats"))

(defun filter-map (f seq)
  (seq-filter #'identity (seq-map f seq)))

(defun in-base-2 (n)
  "Return the decomposition of N in base 2."
  (let ((decomposition '()))
    (while (> n 0)
      (let ((quotient (/ n 2))
            (remainder (% n 2)))
        (setq decomposition (cons remainder decomposition))
        (setq n quotient)))
    
    decomposition))

(defun allergen-list (n)
  (filter-map
   (lambda (pair) (if (car pair) (cdr pair) nil))
   (seq-mapn
    #'cons
    (reverse (seq-map (lambda (x) (if (= 1 x) t nil)) (in-base-2 n)))
    allergens)))

(defun allergic-to-p (score allergen)
  (seq-contains (allergen-list score) allergen 'equal))

(provide 'allergies)
;;; allergies.el ends here
