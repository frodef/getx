
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
	      (loop for y = x then (cddr y)
		    while y
		      thereis (when (eq (car y) (car indicators)) (cadr y)))
	      (cdr indicators)))))
(declaim (notinline ?))

(defmethod documentation ((x (eql '?)) (doc-type (eql 'function)))
  "Inject special indicators into docstring for GETX:?"
  (format nil (documentation #'? 'function) *special-getx-operators*))

(defmacro define-getx (name surface-lambda &body options-body)
  "Define a GETX special indicator. This consists of two functions:
The query function %<NAME> that performs the relevant query,
and the specifier function <NAME> that merely records the indicator
for perusal by GETX:?.

In SURFACE-LAMBDA, the first argument is the current object X, and the
remaining arguments relate to the indicator.

The option QUERY-LAMBDA must be specified if SURFACE-LAMBDA has
non-required arguments. QUERY-LAMBDA must consist only of required
arguments, and the first argument must be identical to that of
SURFACE-LAMBDA."
  (let* ((do-query-name
	     (intern (format nil "%~A" name)))
	 (body
	   (loop for b = options-body then (cddr b)
		   thereis (unless (keywordp (first b)) b)))
	 (options
	   (ldiff options-body body))
	 (docstrings
	   (loop while (and (stringp (car body))
			    (cdr body))
		 collect (pop body)))
	 (declarations
	   (loop while (typep (car body) '(cons (eql declare)))
		 collect (pop body))))
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
	   ,@docstrings
	   ,@declarations
	   (flet ((proceed (new-plist)
		    (apply #'? new-plist continuation)))
	     (declare (ignorable (function proceed)))
	     ,@body))
	 (defun ,name ,(cdr surface-lambda)
	   ,@(when docstrings
	       (list (concatenate 'string "Special GETX indicator: " (first docstrings))))
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
  "Take the Nth element of SEQUENCE, or NIL if out of range. Negative index counts from the end."
  (proceed
   (etypecase sequence
     (list
      (if (minusp n)
	  (car (last sequence (- n)))
	  (nth n sequence)))
     (vector
      (let ((length (length sequence)))
	(cond
	  ((< -1 n length)
	   (aref sequence n))
	  ((< -1 (+ length n) length)
	   (aref sequence (+ length n)))))))))

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
  (if continuation ; PROCEED will discard any multiple-values.
      (proceed (apply #'? x indicators))
      (apply #'? x indicators)))

(define-getx multiple (x &rest indicators)
  :query-lambda (x indicators)
  "Return a list, being multiple INDICATORS from X."
  (mapcar (lambda (indicator)
	    (proceed (? x indicator)))
	  indicators))

(define-getx multiple* (x &rest indicators)
  :query-lambda (x indicators)
  "Return multiple INDICATORS from X, as multiple VALUES."
  (cond
    (t (values-list (mapcar (lambda (indicator)
			      (proceed (? x indicator)))
			    indicators)))))

(define-getx all (plist each-key)
  "Return a list of all values of PLIST for KEY (as opposed to just
returning the first value for KEY)."
  (loop for (key value) on plist by #'cddr
	when (eq key each-key)
	  collect (proceed value)))

(define-getx each (list)
  "Fan out query across each element of LIST. Returns a list."
  (mapcar #'proceed list))

(define-getx keep (list &optional (key 'identity))
  "Fan out query across each non-NIL element of LIST. Returns a list."
  :query-lambda (list key)
  (loop for x in list
	for v = (proceed x)
	when (funcall key v)
	  collect v))

(define-getx each-key (x)
  "Return the keys of a plist or hash-table X, discarding the values."
  (etypecase x
    (list
     (loop for (key value) on x by #'cddr
	   collect (proceed key)))
    (hash-table
     (loop for key being the hash-keys of x
	   collect key))))

(define-getx each-value (x &rest indicators)
  :query-lambda (x indicators)
  "Return the values of PLIST for which query INDICATORS is true, discarding the keys."
  (etypecase x
    (list
     (loop for (key value) on x by #'cddr
	   when (apply #'? value indicators)
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
  (proceed (cdr (assoc item alist :key key :test test))))

(define-getx filter (list function)
  "Map FUNCTION over LIST. Returns a list."
  (proceed (mapcar function list)))

(define-getx combine (list function &optional initial-value)
  :query-lambda (list function initial-value)
  "Combine (i.e. CL:REDUCE) LIST by FUNCTION."
  (proceed (reduce function list :initial-value initial-value)))

(define-getx except (x &rest except-keys)
  :query-lambda (x except-keys)
  "Remove EXCEPT-KEYS from PLIST."
  (etypecase x
    (list
     (let ((new-plist (copy-list x)))
       (dolist (except-key except-keys)
	 (remf new-plist except-key))
       (proceed new-plist)))))

(define-getx yield (x value)
  "Always yield VALUE."
  (declare (ignore x))
  value)

(define-getx call (x f &rest args)
  :query-lambda (x f args)
  "Apply F to X."
  (proceed (apply f x args)))
