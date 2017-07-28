; symbol starts with :
; property list (list :a 1 :b 3)
; getf ([list] [symbol]) - retrieve record

(defun make-cd (title artist rating ripped)
  "make a cd list"
  (list :title title :artist artist :rating rating :ripped ripped))

; defvar [name] [value] - define a global variable
(defvar *db* nil)

; push [item] [target list] - add items to list
(defun add-record (cd)
  (push cd *db*))

; dolist ([item-name list])- binding items
; format [stream] [format] [input-item]- format result
; ~ - directives start with ~
; ~a - argument
; ~[number]t - spacing
; ~{} - a list inside {}
; ~% - new line
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd))) ; t - *standard-output*

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

; log some text to terminal
(defun dd (output)
  (format *query-io* output)
  (force-output *query-io*))

; loop [action] - repeatedly executes a body of expressions
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Repped [y/n]:")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]:"))
	 (return)))) ; return means done the loop

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
	(setf *db* (read in)))))

(defun save-db (filename)
  (with-open-file (out filename
                  :direction :output
                  :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun select (select-fn)
  (remove-if-not select-fn *db*))

(defun select-by (prop value)
  (select #'(lambda (cd) (equal (getf cd prop) value))))

(defun select-by-artist (artist)
  (select-by :artist artist))

; supplied-p parameter - true/false depending on whether an argument
; was actually passed for that keyword
; &key - specifying what parameters are passing to a function
#|
(defun where (&key title artist rating (ripped nil ripped-p))
  (and
   (if title (equal (getf cd :title) title) t)
   (if artist (equal (getf cd :artist) artist) t)
   (if rating (equal (getf cd :rating) rating) t)
   (if ripped-p (equal (getf cd :ripped) t))))
|#
; in a back-quoted expression, any subexpression that's preceded by a comma is evaluated
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
    collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest query)
  `#'(lambda (cd) (and ,@(make-comparisons-list query))))

(defun update (select-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
  (mapcar
   #'(lambda (cd)
       (when (funcall select-fn cd)
         (if title (setf (getf cd :title) title))
         (if artist (setf (getf cd :artist) artist))
         (if rating (setf (getf cd :rating) rating))
         (if ripped-p (setf (getf cd :ripped) ripped-p))))
   *db*)))

; marco - a mechanism for creating abstractions at the syntactic level
(defmacro backwards (expr) (reverse expr))
