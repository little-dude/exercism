;;; allergies.el --- Allergies Exercise (exercism)

;;; Commentary:

;; taken from
;; https://exercism.io/tracks/elisp/exercises/allergies/solutions/6498c769926b49b19cfa290e8f0385d4

;;; Code:
(setq allergen-names '("eggs"
                       "peanuts"
                       "shellfish"
                       "strawberries"
                       "tomatoes"
                       "chocolate"
                       "pollen"
                       "cats"))

(defun allergen-list (score)
  (seq-filter (lambda (a) (allergic-to-p score a)) allergen-names))

(defun allergic-to-p (score allergy)
  (/= 0 (logand score (lsh 1 (seq-position allergen-names allergy)))))

(provide 'allergies)
;;; allergies.el ends here
