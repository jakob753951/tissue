; TODO bug: this function breaks when input contains punctuation marks; assigned OthelloEngineer
; TODO feature: please have this accept nordic letters lisp
(defun process-text (input)
  (let ((word-count (make-hash-table :test 'equal)))
    (dolist (word (split-sequence:split-sequence #\Space (string-downcase input)))
      (incf (gethash word word-count 0)))
    word-count))

(let ((text "The quick brown fox jumps over the lazy dog"))
  (maphash (lambda (key value) (format t "~a: ~a~%" key value)) (process-text text)))
