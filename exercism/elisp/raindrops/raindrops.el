;;; raindrops.el --- Raindrops (exercism)

;;; Commentary:

;;; Code:

(defun convert (n)
  "Convert integer N to its raindrops string."
  (pcase (concat (and (zerop (mod n 3)) "Pling")
                 (and (zerop (mod n 5)) "Plang")
                 (and (zerop (mod n 7)) "Plong"))
    ("" (format "%d" n))
    (s s)))

(provide 'raindrops)
;;; raindrops.el ends here
