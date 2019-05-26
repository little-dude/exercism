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
  (let ((pos 0)
        (regexp "\\([0-9]*\\)\\([^0-9]\\)")
        (result ""))
    (while (string-match regexp s pos)
      (let ((count (string-to-number (match-string 1 s)))
            (char (match-string 2 s)))
        (if (zerop count)
            (setq result (concat result char))
          (setq result (concat result (make-string count (string-to-char char))))))
      ;; next match will start after the last match
      (setq pos (match-end 0)))
    result))

(provide 'run-length-encoding)
;;; run-length-encoding.el ends here
