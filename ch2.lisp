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

(setf *grammar* *simple-grammar*)

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

(defun generate-2 (phrase)
	(if (listp phrase)
		(mappend #'generate phrase)
		(let ((choices (rewrites phrase)))
			(if (null choices)
				(list phrase)
				(generate-2 (random-elt choices))))))

(defparameter *bigger-grammar*
	'((sentence -> (noun-phrase verb-phrase))
		(noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
		(verb-phrase -> (Verb noun-phrase PP*))
		(PP* -> () (PP PP*))
		(Adj* -> () (Adj Adj*))
		(PP -> (Prep noun-phrase))
		(Prep -> to in by with on)
		(Adj -> big little blue green adiabatic)
		(Article -> the a)
		(Name -> Pat Kim Lee Terry Robin)
		(Noun -> man ball woman table)
		(Verb ->hit took saw liked)
		(Pronoun -> he she it these those that)))

(setf *grammar* *bigger-grammar*)

(defun generate-tree (phrase)
	(if (listp phrase)
		(mapcar #'generate-tree phrase)
		(let ((choices (rewrites phrase)))
			(if (null choices)
				(list phrase)
				(cons phrase (generate-tree (random-elt (rewrites phrase))))))))

(defun combine-all (xlist ylist)
	(mappend #'(lambda (y)
								(mapcar #'(lambda (x) (append x y)) xlist))
					 ylist))

(defun generate-all (phrase)
	(cond ((null phrase) (list nil))
				((listp phrase)
					(combine-all (generate-all (first phrase))
											 (generate-all (rest phrase))))
				((rewrites phrase)
					(mappend #'generate-all (rewrites phrase)))
				(t (list (list phrase)))))
