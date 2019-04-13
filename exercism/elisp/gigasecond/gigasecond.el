;;; gigasecond.el --- Gigasecond exercise (exercism)

;;; Commentary:
;; Calculate the date one gigasecond (10^9 seconds) from the
;; given date.
;;
;; NB: Pay attention to  Emacs' handling of time zones and dst
;; in the encode-time and decode-time functions.

;;; Code:

(defun from (s m h D M Y)
  (set-time-zone-rule t)
  (let ((gigasecond (expt 10 9))
        (start-time (encode-time s m h D M Y t)))
    (seq-subseq
     (decode-time (time-add start-time gigasecond) t)
     0 6)))

(provide 'gigasecond)
;;; gigasecond.el ends here
