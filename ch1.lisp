(defparameter *titles*
  '(Mr Mrs Miss Ms S i r Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

(defun first-name (name)
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the result"
  (apply #'append (mapcar fn the-list)))

(setf p '(John Q Public))

(setf names '((John Q Public) (John Macthey)))


(defun double-me (x)
  (* x 2))

(setf lst (mapcar #'double-me '(1 2 3)))


(funcall #'+ 1 3)
(apply #'+ '(1 3))

(set qq (apply #'(lambda (x) (* x x)) '(1 3)))
