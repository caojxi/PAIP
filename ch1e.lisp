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
