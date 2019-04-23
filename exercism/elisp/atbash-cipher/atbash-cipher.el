;;; atbash-cipher.el --- Atbash-Cipher (exercism)

;;; Commentary:




;;; Code:

(defun sanitize-string (s)
  "Convert S to lowercase and remove non-letter characters."
  (seq-filter
   (lambda (c) (<= ?a c ?z))
   (downcase s)))

(defun encode-letters (letters)
  "Encode the given sequence of characters LETTERS.
The sequence must contain only lowercase characters that
represent latin letters \"a\" to \"z\"."
  (let* ((alphabet "abcdefghijklmnopqrstuvwxyz")
         (tebahpla (reverse alphabet)))
    (seq-map
     (lambda (c) (elt tebahpla (seq-position alphabet c '=)))
     letters)))

(defun encode (plaintext)
  "Encode PLAINTEXT to atbash-cipher encoding."
  (mapconcat
   'identity
   (seq-partition (encode-letters (sanitize-string plaintext)) 5)
   " "))

(provide 'atbash-cipher)
;;; atbash-cipher.el ends here
