(defun random-elt (choices)
	"choose an element from a list at random"
	(elt choices (random (length choices))))

(defun one-of (set)
	"Pick one element of set, and make a list of it"
	(list (random-elt set)))

(defun Adj* ()
	(if (= (random 2) 0)
		nil
		(append (Adj) (Adj*))))

(defun PP* ()
	(if (random-elt '(t nil))
		nil
		(append (PP) (PP*))))

(defun Noun ()
	(one-of '(man ball woman table)))

(defun noun-phrase ()
	(append (Article) (Adj*) (Noun) (PP*)))

(defun Article ()
	(one-of '(the a)))

(defun Adj ()
	(one-of '(big little blue green adiabatic)))

(defun PP ()
	(append (Prep) (noun-phrase)))

(defun Prep ()
	(one-of '(to in by with on)))

(defun Verb ()
	(one-of '(hit took saw liked)))

(defparameter *simple-grammar*
	'((sentence -> (noun-phrase verb-phrase))
		(noun-phrase -> (Article Noun))
		(verb-phrase -> (Verb noun-phrase))
		(Article -> the a)
		(Noun -> man ball woman table)
		(verb -> bit took saw liked))
	"A grammar for a trivial subset of English")

(defvar *grammar* *simple-grammar*)

(assoc 'noun *grammar*)

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the result"
  (apply #'append (mapcar fn the-list)))

(defun rule-lhs (rule)
	(first rule))

(defun rule-rhs (rule)
	(rest (rest rule)))

(defun rewrites (category)
	"Return a list of the possible rewrites for this category"
	(rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
	(cond ((listp phrase)
					(mappend #'generate phrase))
				((rewrites phrase)
					(generate (random-elt (rewrites phrase))))
				(t (list phrase))))
