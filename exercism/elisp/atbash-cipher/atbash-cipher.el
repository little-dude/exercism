;;; atbash-cipher.el --- Atbash-Cipher (exercism)

;;; Commentary:




;;; Code:
(defun encode (plaintext)
  "Encode PLAINTEXT to atbash-cipher encoding."
  (substring
   (seq-reduce
    (lambda (cipher-text chars) (concat cipher-text " " chars))
    (seq-partition
     (seq-map
      (lambda (c) (elt reversed-alphabet (seq-position alphabet c '=)))
      (seq-filter
       (lambda (c) (<= ?a c ?z))
       (downcase plaintext)))
     5)
    "")
   1
   nil))

(provide 'atbash-cipher)
;;; atbash-cipher.el ends here
