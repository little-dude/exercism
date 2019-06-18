;;; phone-number.el --- phone-number Exercise (exercism)

;;; Commentary:

;;; Code:

(defun numbers (number)
  "Return the ten digits phone number corresponding to NUMBER.
If NUMBER is an invalid phone number, return \"0000000000\"."
  (let* ((digits (replace-regexp-in-string "[^0-9]" "" number))
         (nb-digits (length digits))
         (starts-with-1 (string-prefix-p "1" digits)))
    (cond
     ((= 10 nb-digits) digits)
     ((and (= 11 nb-digits) starts-with-1) (substring digits 1 11))
     (t "0000000000"))))

(defun area-code (number)
  "Return the three first digits of NUMBER."
  (substring (numbers number) 0 3))

(defun pprint (number)
  "Return a pretty formatted phone number from NUMBER."
  (replace-regexp-in-string
   "\\([0-9]\\{3\\}\\)\\([0-9]\\{3\\}\\)\\([0-9]\\{4\\}\\)"
   "(\\1) \\2-\\3"
   (numbers number)))

(provide 'phone-number)
;;; phone-number.el ends here
