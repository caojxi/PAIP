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


					; Exercise 1
					; Define a version of last-name that handles "Rex Morgan MD," "Morton Downey, Jr.," and whatever other cases you can think of.
					; Exercise 2
					; Write a function to exponentiate, or raise a number to an integer power. For example: (power 3 2 ) = 9.
					; Exercise 3
					; Write a function that counts the number of atoms in an expression.
					; For example:(count-atoms '(a (b) c)) = 3. Notice that there is something of an ambiguity in this:
					; should ( a nil c ) count as three atoms, or as two, because it is equivalent to (a () c)?
					; Exercise 4
					; Write a function that counts the number of times an expression occurs anywhere within another expression.
					; Example: (count-anywhere 'a '(a ((a) b) a)) => 3.
					; Exercise 5
					; Write a function to compute the dot product of two sequences of numbers, represented as lists.
					; The dot product is computed by multiplying corresponding elements and then adding up the resulting products. Example:
					; (dot-product'(1020) '(34)) = 10x3 +20x4 = 110


(set qq (apply #'(lambda (x) (* x x)) '(1 3)))
