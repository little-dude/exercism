;;; robot-name.el --- Robot Name (exercism)

;;; Commentary:
;;
;; Build a robot with a name like AA000, that can be reset
;; to a new name. Instructions are in README.md
;;

;;; Code:

;; initialize the pseudo-random number generator with a new seed
(random t)

(cl-defstruct robot name)

(defun random-name ()
  (format
   "%c%c%d"
   (+ ?A (random 26))
   (+ ?A (random 26))
   (+ 100 (random 900))))

(defun build-robot ()
  "Return a robot with a random name."
  (make-robot :name (random-name)))

(defun reset-robot (robot)
  "Reset the given ROBOT. The new robot will have a different name."
  (setf (robot-name robot) (random-name)))


(provide 'robot-name)
;;; robot-name.el ends here
