
(in-package #:getx)

(defparameter *special-getx-operators* nil
  "Records a list of operators defined by DEFINE-GETX.")

(declaim (inline ?))
(defun ? (data &rest indicators)
  "Query a hierarchical data-structure DATA by recursively applying
INDICATORS sequentially.

If INDICATOR is an integer, its is used as an index into DATA. A
negative index counts from the end.

If INDICATOR is a function object, that function is applied to DATA.

If INDICATOR is any other atom (typically a keyword or symbol) it is
either looked up in DATA by CL:GETF, CL:GETHASH, or CL:SLOT-VALUE,
depending on the type of DATA.

Otherwise, INDICATOR is a special indicator form that can operate on
any number of data types for DATA. ~@[ See the documentation for the
individual special indicators: ~{~S~^, ~}.~]

For example, to find the email address and phone number of every
employee in some Acme company whose first name is Frode:
  (getx:? company-plists (getx:seek :name \"Acme\" 'str:contains?) :employees (getx:select :first-name \"Frode\") (getx:multiple :email :phone))"
  (if (null indicators)
      data ; Query completed.
      (let ((indicator (car indicators)))
	(typecase indicator
	  (null
	   (apply #'? data (cdr indicators)))
	  ((integer 0 *)
	   ;; integer index indicator
	   (apply #'?
		  (etypecase data
		    (list
		     (dotimes (i indicator (car data))
		       (pop data)
		       (when (atom data)
			 (error 'type-error :datum data :expected-type 'list))))
		    (vector
		     (aref data indicator)))
		  (cdr indicators)))
	  ((integer * -1)
	   ;; negative integer index indicator, count from end
	   (let ((n (- indicator)))
	     (apply #'?
		    (etypecase data
		      (list
		       (let ((y data))
			 (dotimes (i n (loop while y do (pop data) (pop y)
					     finally (return (car data))))
			   (unless y (return nil))
			   (pop y))))
		      (vector
		       (let ((l (length data)))
			 (if (<= n l)
			     (aref data (- l n))
			     nil))))
		    (cdr indicators))))
	  (function
	   ;; Function indicator
	   (apply #'?
		  (funcall indicator data)
		  (cdr indicators)))
	  (cons
	   ;; A special indicator form
	   (apply (caar indicators)
		  (cdr indicators)
		  data
		  (cdar indicators)))
	  (t (etypecase data
	       (hash-table
		(apply #'?
		       (gethash indicator data)
		       (cdr indicators)))
	       ((or standard-object structure-object)
		(apply #'?
		       (slot-value data indicator)
		       (cdr indicators)))
	       (list
		;; GETF-like query
		(apply #'?
		       (loop for y = data then (cddr y)
			     while y
			       thereis (when (eq (car y) indicator) (cadr y)))
		       (cdr indicators)))))))))
(declaim (notinline ?))

(defmethod documentation ((x (eql '?)) (doc-type (eql 'function)))
  "Inject special indicators into docstring for GETX:?"
  (format nil (documentation #'? 'function) *special-getx-operators*))

(defmacro define-getx (name surface-lambda &body options-body)
  "Define a GETX special indicator. This consists of two functions:
The query function %<NAME> that performs the relevant query, and the
surface syntax function <NAME> that merely records the indicator for
perusal by GETX:?.

In SURFACE-LAMBDA, the first argument is the current data object X,
and the remaining arguments are processed by the query function.

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

(defmacro ?? (data &rest indicators)
  `(locally (declare (inline ? ,@*special-getx-operators*))
     (? ,data ,@indicators)))

(define-getx seek (list indicator value &optional (test 'equal))
  :query-lambda (list indicator value test)
  "Find the first element of LIST where INDICATOR matches VALUE under TEST."
  (dolist (element list)
    (when (funcall test value (? element indicator))
      (return (proceed element)))))

(define-getx select* (list indicator value &optional (test 'equal))
  :query-lambda (list indicator value test)
  "Select each element of LIST where INDICATOR matches VALUE under TEST. Fan out query."
  (mapcan (lambda (element)
	    (when (funcall test value (? element indicator))
	      (list (proceed element))))
	  list))

(define-getx progn? (data &rest indicators)
  :query-lambda (data indicators)
  "Process a sequence of INDICATORS, i.e. recursively call ?."
  (if continuation ; PROCEED will discard any multiple-values.
      (proceed (apply #'? data indicators))
      (apply #'? data indicators)))

(define-getx multiple (data &rest indicators)
  :query-lambda (data indicators)
  "Return a list, being multiple INDICATORS from DATA."
  (mapcar (lambda (indicator)
	    (proceed (? data indicator)))
	  indicators))

(define-getx multiple-values (data &rest indicators)
  :query-lambda (data indicators)
  "Return multiple INDICATORS from DATA, as multiple values."
  (values-list (mapcar (lambda (indicator)
			 (proceed (? data indicator)))
		       indicators)))

(define-getx all* (plist each-key)
  "Fan out across a list of all values of PLIST for KEY (as opposed to
just returning the first value for KEY)."
  (loop for (key value) on plist by #'cddr
	when (eq key each-key)
	  collect (proceed value)))

(define-getx foreach* (list)
  "Fan out query across each element of LIST. Returns a list."
  (mapcar #'proceed list))

(define-getx keep* (list &optional (key 'identity))
  :query-lambda (list key)
  "Fan out query across each element of LIST for which KEY is
non-NIL."
  (loop for x in list
	when (funcall key x)
	  collect (proceed x)))

(define-getx keep-type* (list type)
  :query-lambda (list type)
  "Fan out query across each element of LIST which matches TYPE."
  (loop for x in list
	when (typep x type)
	  collect (proceed x)))

(define-getx each-key* (data)
  "Return the keys of a plist or hash-table DATA, discarding the
values."
  (etypecase data
    (list
     (loop for (key value) on data by #'cddr
	   collect (proceed key)))
    (hash-table
     (loop for key being the hash-keys of data
	   collect (proceed key)))))

(define-getx each-value* (data &rest indicators)
  :query-lambda (data indicators)
  "Fan out across the values of plist or hash-table DATA for which
query INDICATORS is true, discarding the keys."
  (etypecase data
    (list
     (loop for (key value) on data by #'cddr
	   when (apply #'? value indicators)
	     collect (proceed value)))
    (hash-table
     (loop for value being the hash-values of data
	   collect (proceed value)))))

(define-getx either (data &rest indicators)
  :query-lambda (data indicators)
  "Return the first INDICATOR sub-query that returns a non-NIL result."
  (loop for indicator in indicators
	  thereis (proceed (? data indicator))))

(define-getx associate (alist item &key (key 'identity) (test 'eq))
  :query-lambda (alist item key test)
  "Look up ITEM in ALIST as if by CL:ASSOC."
  (proceed (cdr (assoc item alist :key key :test test))))

(define-getx filter (list function)
  "Map FUNCTION over LIST. Returns a list."
  (proceed (mapcar function list)))

(define-getx except (data &rest except-keys)
  :query-lambda (data except-keys)
  "Remove EXCEPT-KEYS (and their values) from DATA, non-destructively."
  (proceed
   (etypecase data
     (list
      (loop for (k v) on data by #'cddr
	    unless (member k except-keys)
	      collect k and collect v))
     (hash-table
      (let ((new-table (make-hash-table :test (hash-table-test data))))
	(maphash (lambda (k v)
		   (unless (member k except-keys)
		     (setf (gethash k new-table) v)))
		 data)
	new-table)))))

(define-getx yield (data value)
  "Always yield VALUE, regardless of DATA."
  (declare (ignore data))
  value)

(define-getx call (list f &rest args)
  :query-lambda (list f args)
  "Apply F to LIST."
  (proceed (apply f list args)))
