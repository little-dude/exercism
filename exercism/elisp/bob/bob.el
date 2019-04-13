;;; bob.el --- Bob exercise (exercism)

;;; Commentary:

;;; Code:


(setq sure "Sure.")
(setq chill-out "Whoa, chill out!")
(setq fine "Fine. Be that way!")
(setq whatever "Whatever.")
(setq calm-down "Calm down, I know what I'm doing!")

(defun is-question? (sentence)
  "Return `t` if SENTENCE end with a question mark."
  (if (string-match-p "\?[\s\t\r\n]*\\'" sentence) t nil))

(defun is-uppercase? (sentence)
  "Return `t` is SENTENCE is uppercased, `nil` otherwise."
  (and
   ;; Make sure the string has at least one char within a-z or A-Z.
   ;; If there are only special characters, we consider the string is
   ;; lowercase. Note that [:alpha:] does not work since it match ":"
   ;; for instance, whereas the doc says it matches only letters.
   (string-match-p "[a-zA-Z]+" sentence)
   (equal sentence (upcase sentence))))

(defun is-empty? (sentence)
  "Return `t` if SENTENCE is empty or made only of whitespaces."
  (if (string-match-p "\\`[\n\t\s\r]*\\'" sentence) t nil))

(defun response-for (sentence)
  "Return the reponse for SENTENCE."
  (cond
   ((is-empty? sentence)
    fine)
   
   ((is-question? sentence)
    (if (is-uppercase? sentence)
        calm-down
      sure))
   
   ((is-uppercase? sentence)
    chill-out)

   (t whatever)))

(provide 'bob)
;;; bob.el ends here
