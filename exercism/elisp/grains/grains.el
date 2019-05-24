;;; grains.el --- Grains exercise (exercism)

;;; Commentary:

;;; Code:

(defun square (n)
  "Return the number of grains of wheat on square N."
  (expt 2 (1- n)))

(defun total ()
  "Return the number of grains of wheat on the chessboard."
  (seq-reduce '+ (seq-map 'square (number-sequence 1 64)) 0))


(provide 'grains)
;;; grains.el ends here
