;;; hello-world.el --- Hello World Exercise (exercism)

;;; Commentary:
;; This package provides a function that prints "Hello, World!"
;; Useful things:
;;     - to run the tests interactively: M-x ert <RET> t <RET>
;;     - repl: M-x ielm (C-c C-b to "follow" a buffer)
;;     - it seems that it's not enough to save the buffer for ielm to be update, we have to do C-M-x

;;; Code:

(defun hello ()
  "Return the string \"Hello, World!\"."
  "Hello, World!")

(provide 'hello-world)

;;; hello-world.el ends here
