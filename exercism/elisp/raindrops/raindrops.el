;;; raindrops.el --- Raindrops (exercism)

;;; Commentary:

;;; Code:

(defun convert (n)
  "Convert integer N to its raindrops string."
  (cond
   ((= 0 (mod n (* 3 5 7))) "PlingPlangPlong")
   ((= 0 (mod n (* 3 7))) "PlingPlong")
   ((= 0 (mod n (* 3 5))) "PlingPlang")
   ((= 0 (mod n (* 5 7))) "PlangPlong")
   ((= 0 (mod n (* 3))) "Pling")
   ((= 0 (mod n (* 5))) "Plang")
   ((= 0 (mod n (* 7))) "Plong")
   (t (format "%d" n))))

(provide 'raindrops)
;;; raindrops.el ends here
