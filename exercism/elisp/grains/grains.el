;;; grains.el --- Grains exercise (exercism)

;;; Commentary:

;;; Code:

(defun square (n)
  "Return the number of grains of wheat on square N."
  (expt 2.0 (1- n)))

(defun total ()
  "Return the number of grains of wheat on the chessboard."
  (square 65))


(provide 'grains)
;;; grains.el ends here
