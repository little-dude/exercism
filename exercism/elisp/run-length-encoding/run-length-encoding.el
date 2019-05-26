;;; run-length-encoding.el --- run-length-encoding Exercise (exercism)

;;; Commentary:

;;; Code:

(require 'seq)

(defun run-length-encode (s)
  "Encode S."
  (cl-loop for c across s
           with current-char ; character being counted
           with count = 0    ; counter for the current character
           with counters     ; an alist to keep track of all the
                             ; characters and their counts

           ;; this case only applies to the first iteration
           if (equal current-char nil) do
             (setq current-char c)
             (setq count 1)

           ;; when the current char is the same than the previous, just increment the counter
           else if (equal c current-char) do
             (setq count (1+ count))

           ;; the current char differs from the previous:
           ;; - store the previous char and its counter
           ;; - reset the counter to 1
           else do
             (setq counters (nconc counters (list (cons current-char count))))
             (setq current-char c)
             (setq count 1)

           ;; after the last iteration, store the last char and its counter and build the result
           finally do
             (setq counters (nconc counters (list (cons current-char count))))
             (cl-return
              (seq-reduce
               (lambda (result counter)
                 (concat result (pcase counter
                                  (`(nil . 0) "") ; this can happen for empty input
                                  (`(,c . 1) (string c))
                                  (`(,c . ,count) (format "%d%c" count c)))))
               counters
               ""))))
                        

(defun run-length-decode (s)
  "Decode S."
  (let ((i 0)
        (result ""))
    (catch 'eos
      (while (< i (length s))
        (let* ((c1 (substring s i (1+ i)))
               (c2 (condition-case nil
                       (substring s (1+ i) (+ 2 i))
                     (error (throw 'eos (concat result c1)))))
               (count (string-to-number c1)))
          (if (zerop count)
              (progn
                (setq result (concat result c1))
                (setq i (1+ i)))
            (progn
              (setq result (concat result (make-string count (string-to-char c2))))
              (setq i (+ 2 i))))))
      result)))

(provide 'run-length-encoding)
;;; run-length-encoding.el ends here
