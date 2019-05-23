;;; raindrops.el --- Raindrops (exercism)

;;; Commentary:

;;; Code:

(defun convert (n)
  "Convert integer N to its raindrops string."
  (cond
   ((zerop (mod n (* 3 5 7))) "PlingPlangPlong")
   ((zerop (mod n (* 3 7))) "PlingPlong")
   ((zerop (mod n (* 3 5))) "PlingPlang")
   ((zerop (mod n (* 5 7))) "PlangPlong")
   ((zerop (mod n (* 3))) "Pling")
   ((zerop (mod n (* 5))) "Plang")
   ((zerop (mod n (* 7))) "Plong")
   (t (format "%d" n))))

(provide 'raindrops)
;;; raindrops.el ends here
