
(in-package #:getx)

(defparameter *special-getx-operators* nil
  "Records a list of operators defined by DEFINE-GETX.")

(declaim (inline ?))
(defun ? (x &rest indicators)
  "Query a hierarchical data-structure X by recursively applying
INDICATORS sequentially.

If INDICATOR is an atom (typically a symbol) it is either looked up in
X by CL:GETF, CL:GETHASH, or CL:SLOT-VALUE, depending on the type of
X.

Otherwise, INDICATOR is a special indicator form that can operate on
any number of data types for X. ~@[ See the documentation for the
individual special indicators: ~{~S~^, ~}.~]

For example, to find the email address and phone number of every
employee in some Acme company whose first name is Frode:
  (getx:? company-plists (getx:seek :name \"Acme\" 'str:contains?) :employees (getx:select :first-name \"Frode\") (getx:multiple :email :phone))"
  (cond
    ((null indicators)
     x)
    ((consp (car indicators))
     (apply (caar indicators)
	    (cdr indicators)
	    x
	    (cdar indicators)))
    ((hash-table-p x)
     (apply #'?
	    (gethash (car indicators) x)
	    (cdr indicators)))
    ((typep x '(or standard-object structure-object))
     (apply #'?
	    (slot-value x (car indicators))
	    (cdr indicators)))
    (t (apply #'?
	      (getf x (car indicators))
	      (cdr indicators)))))
(declaim (notinline ?))

(defmethod documentation ((x (eql '?)) (doc-type (eql 'function)))
  "Inject special indicators into docstring for GETX:?"
  (format nil (documentation #'? 'function) *special-getx-operators*))

(defmacro define-getx (name surface-lambda &body options-body)
  "Define a GETX special indicator. This consists of two functions:
The query function %<NAME> that performs the relevant query,
and the specifier function <NAME> that merely records the indicator
for perusal by GETX:?

In SURFACE-LAMBDA, the first argument is the current object X, and the
remaining arguments relate to the indicator."
  (let* ((do-query-name
	     (intern (format nil "%~A" name)))
	 (body
	   (loop for b = options-body then (cddr b)
		   thereis (unless (keywordp (first b)) b)))
	 (options
	   (ldiff options-body body)))
    (destructuring-bind (&key (query-lambda surface-lambda))
	options
      (assert (and (every #'symbolp query-lambda)
		   (null (intersection query-lambda lambda-list-keywords)))
	      (query-lambda)
	      "The ~S must only include required arguments: ~S" 'query-lambda query-lambda)
      (assert (eq (first surface-lambda)
		  (first query-lambda))
	      (surface-lambda query-lambda)
	      "The ~S and ~S must share the same first argument (for the object X)."
	      'surface-lambda 'query-lambda)
      `(progn
	 (pushnew ',name *special-getx-operators*)
	 (declaim (inline ,name ,do-query-name))
	 (defun ,do-query-name (continuation ,@query-lambda)
	   (flet ((proceed (new-plist)
		    (apply #'? new-plist continuation)))
	     ,@(if (stringp (first body))
		   (cdr body)
		   body)))
	 (defun ,name ,(cdr surface-lambda)
	   ,@(when (stringp (first body))
	       (list (concatenate 'string "Special GETX indicator: " (first body))))
	   (list ',do-query-name ,@(cdr query-lambda)))
	 (declaim (notinline ,name ,do-query-name))))))

(defmacro ?? (x &rest indicators)
  `(locally (declare (inline ? ,@*special-getx-operators*))
     (? ,x ,@indicators)))

(define-getx seek (list indicator value &optional (test 'equal))
  :query-lambda (list indicator value test)
  "Find the first element of LIST where INDICATOR matches VALUE under TEST."
  (dolist (element list)
    (when (funcall test value (? element indicator))
      (return (proceed element)))))

(define-getx index (sequence n)
  "Take the Nth element of SEQUENCE, or NIL if out of range."
  (proceed
   (etypecase sequence
     (list
      (nth n sequence))
     (vector
      (when (<= 0 n (length sequence))
	(aref sequence n))))))

(define-getx select (list indicator value &optional (test 'equal))
  :query-lambda (list indicator value test)
  "Select each element of LIST where INDICATOR matches VALUE under TEST."
  (mapcan (lambda (element)
	    (when (funcall test value (? element indicator))
	      (list (proceed element))))
	  list))

(define-getx prog? (x &rest indicators)
  :query-lambda (x indicators)
  "Process a sequence of INDICATORS."
  (proceed (apply #'? x indicators)))

(define-getx multiple (x &rest indicators)
  :query-lambda (x indicators)
  "Return a list, being multiple INDICATORS from X."
  (mapcar (lambda (indicator)
	    (proceed (? x indicator)))
	  indicators))

(define-getx each (plist each-key)
  "Return a list of all values of PLIST for KEY (as opposed to just
returning the first value for KEY)."
  (loop for (key value) on plist by #'cddr
	when (eq key each-key)
	  collect (proceed value)))

(define-getx each-key (x)
  "Return the keys of a plist or hash-table X, discarding the values."
  (etypecase x
    (list
     (loop for (key value) on x by #'cddr
	   collect (proceed key)))
    (hash-table
     (loop for key being the hash-keys of x
	   collect key))))

(define-getx each-value (x)
  "Return the values of PLIST, discarding the keys."
  (etypecase x
    (list
     (loop for (key value) on x by #'cddr
	   collect (proceed value)))
    (hash-table
     (loop for value being the hash-values of x
	   collect value))))

(define-getx either (x &rest indicators)
  :query-lambda (x indicators)
  "Return the first INDICATOR that returns a non-NIL result."
  (loop for indicator in indicators
	thereis (proceed (? x indicator))))

(define-getx associate (alist item &key (key 'identity) (test 'eq))
  :query-lambda (alist item key test)
  "Look up ITEM in ALIST as by CL:ASSOC."
  (proceed (assoc item alist :key key :test test)))

(define-getx filter (list function)
  "Map FUNCTION over LIST."
  (proceed (mapcar function list)))
