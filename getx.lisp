(in-package #:getx)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *special-getx-operators* nil
    "Records a list of operators defined by DEFINE-GETX."))

(defvar *standard-object-plist* nil
  "Function to optionally map STANDARD-OBJECT data to plist. Function is
applied to DATA and first INDICATOR, and can decline by returning the
same object.")

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

If INDICATOR is a string, DATA is formattet by that string.

For example, to find the email address and phone number of every
employee in some Acme company whose first name is Frode:
  (getx:? company-plists (getx:seek :name \"Acme\" 'str:contains?) :employees (getx:select :first-name \"Frode\") (getx:listing :email :phone))"
  (declare (dynamic-extent indicators))
  (if (null indicators)
      (values data t)				; Query completed.
      (let ((indicator (car indicators)))
	(typecase indicator
	  (null
	   (apply #'? data (cdr indicators)))
	  ((and (integer 0 *) fixnum)
	   ;; integer index indicator
	   (apply #'?
		  (etypecase data
		    (cons
		     (dotimes (i indicator (car data))
		       (pop data)
		       (when (atom data)
			 (error 'type-error :datum data :expected-type 'list))))
		    (vector
		     (if (< indicator (length data))
			 (aref data indicator)
			 nil)))
		  (cdr indicators)))
	  ((and (integer * -1) fixnum)
	   ;; negative integer index indicator, count from end
	   (let ((n (- indicator)))
	     (apply #'?
		    (etypecase data
		      (cons
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
	  #+ignore (string
		    (apply #'? (format nil indicator data)
			   (cdr indicators)))
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
	  (stream
	   ;; PRINC data to stream
	   (princ data indicator))
	  (t (typecase data
	       (hash-table
		(apply #'? (gethash indicator data)
		       (cdr indicators)))
	       ((or standard-object structure-object)
		(let ((data2 (if (not *standard-object-plist*)
				 data
				 (funcall *standard-object-plist* data (car indicators)))))
		  (if (eq data data2)
		      (apply #'?
			     (slot-value data indicator)
			     (cdr indicators))
		      (apply #'?
			     data2
			     indicators))))
	       (list
		;; GETF-like query
		(apply #'?
		       (getf data indicator)
		       (cdr indicators)))
	       (t (cond
		    ((stringp indicator)
		     (apply #'? (format nil indicator data)
			    (cdr indicators)))
		    (t (error "Unknown ~S data/indicator combination: ~S / ~S" '? (type-of data) (type-of indicator)))))))))))
(declaim (notinline ?))

(defmethod documentation ((x (eql '?)) (doc-type (eql 'function)))
  "Inject special indicators into docstring for GETX:?"
  (format nil (documentation #'? 'function) *special-getx-operators*))

(defun p? (&rest indicators)
  "A predicate variant of ?, returns a function that applies INDICATORS
to its argument."
  (let ((sop *standard-object-plist*))
    (lambda (data)
      (let ((*standard-object-plist* sop))
	(apply #'? data indicators)))))


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
	   (declare (inline ?))
	   (macrolet ((proceed (new-plist)
			#+ignore `(apply #'? ,new-plist continuation)
			`(if continuation
			     (apply #'? ,new-plist continuation)
			     ,new-plist)))
	     ;; (declare (ignorable (function proceed)))
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

(define-getx unselect (list $indicator $compare &optional (test 'equal))
  :query-lambda (list $indicator $compare test)
  "Select each element of LIST where $INDICATOR doesn't match
LIST:$COMPARE under TEST. Fan out query."
  (let ((compare (? list $compare)))
    (mapcan (lambda (element)
	      (unless (funcall test (? element $indicator) compare)
		(list (proceed element))))
	    list)))

(define-getx select (list indicator value &optional (test 'equal))
  :query-lambda (list indicator value test)
  (proceed (loop for element in list
		 when (funcall test value (? element indicator))
		   collect element)))

(define-getx progn? (data &rest indicators)
  :query-lambda (data indicators)
  "Process a sequence of INDICATORS, i.e. recursively call ?."
  (proceed (apply #'? data indicators)))

(define-getx either (data &rest indicators)
  :query-lambda (data indicators)
  "Proceed with first INDICATOR that is non-nil."
  (loop for indicator in indicators
	for sub-data = (? data indicator)
	when sub-data
	  do (return (proceed sub-data))))

(define-getx listing (data &rest sub-queries)
  :query-lambda (data sub-queries)
  "Proceed with a list of the result of each SUB-QUERY."
  (proceed
   (mapcar (lambda (sub-query)
	     (? data sub-query))
	   sub-queries)))

(define-getx listing* (data &rest sub-queries)
  :query-lambda (data sub-queries)
  "Proceed with a list of the result of each SUB-QUERY, with the result of the
last SUB-QUERY being the tail of the list, like CL:LIST*."
  (proceed
   (loop for (sub-query . more-queries-p) on sub-queries
	 ;; do (warn "i ~S more ~S" indicator more-indicators-p)
	 if more-queries-p
	   collect (? data sub-query)
	 else
	   nconc (? data sub-query))))

(define-getx multiple-values (data &rest indicators)
  :query-lambda (data indicators)
  "Return multiple INDICATORS from DATA, as multiple values."
  (values-list (mapcar (lambda (indicator)
			 (proceed (? data indicator)))
		       indicators)))

(define-getx all (plist plist-indicator)
  "Make a list of all values of PLIST for PLIST-INDICATOR (as opposed to
just returning the first value for KEY)."
  (proceed
   (loop for (k v) on plist by #'cddr
	 when (eq plist-indicator k)
	   collect v)))

(define-getx fork (list)
  "Fork out query across each element of LIST. Returns a list. Note that
the remaining query will be executed (length LIST) times."
  (loop for element in list
	collect (proceed element)))

(define-getx keep* (list &optional (key 'identity))
  :query-lambda (list key)
  "Fan out query across each element of LIST for which KEY is
non-NIL."
  (loop for x in list
	when (funcall key x)
	  collect (proceed x)))

(define-getx keep (list &rest implicit-prog?)
  :query-lambda (list implicit-prog?)
  "Proceed with the elements of LIST for which the sub-query is true."
  (proceed
   (loop for x in list
	 when (apply #'? x implicit-prog?)
	   collect x)))

(define-getx each-key (data &rest $sub-query)
  :query-lambda (data $sub-query)
  "Proceed query with a list of the SUB-QUERY applied to the keys of a
plist or hash-table DATA, discarding the values."
  (proceed
   (etypecase data
     (list
      (loop for (key value) on data by #'cddr
	    collect (apply #'? key $sub-query)))
     (hash-table
      (loop for key being the hash-keys of data
	    collect (apply #'? key $sub-query))))))

(define-getx each-value (data &rest $sub-query)
  :query-lambda (data $sub-query)
  "Proceed query with a list of the SUB-QUERY applied to the values of a
plist or hash-table DATA, ignoring the keys."
  (proceed
   (etypecase data
     (list
      (loop for (key value) on data by #'cddr
	    collect (apply #'? value $sub-query)))
     (hash-table
      (loop for value being the hash-values of data
	    collect (apply #'? value $sub-query))))))

#+ignore (define-getx either (data &rest indicators)
	   :query-lambda (data indicators)
	   "Return the first INDICATOR sub-query that returns a non-NIL result."
	   (loop for indicator in indicators
		   thereis (proceed (? data indicator))))

(define-getx associate (alist item &key (key 'identity) (test 'eq))
  :query-lambda (alist item key test)
  "Look up ITEM in ALIST as if by CL:ASSOC."
  (proceed (assoc item alist :key key :test test)))

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
  (proceed (values value 'yield)))

(define-getx call (data f &rest $args)
  :query-lambda (data f $args)
  "Apply F to DATA and any ARGS."
  (proceed (apply f (mapcar (lambda (arg) (? data arg)) $args))))

(define-getx call* (list f &rest args)
  :query-lambda (list f args)
  "Apply F to the elements of LIST and any ARGS."
  (proceed (apply f (append list args))))

(define-getx fmt (data formatter &optional (first-indicator #'identity) &rest more-indicators)
  :query-lambda (data formatter first-indicator more-indicators)
  "Format DATA into a string. Each string FORMATTER is passed to
CL:FORMAT with any succeeding non-string indicators as arguments,
until there are no more FORMATTERS."
  (proceed
   (apply #'format nil formatter
	  (? data first-indicator)
	  (mapcar (lambda (indicator)
		    (? data indicator))
		  more-indicators))
   #+ignore
   (with-output-to-string (out)
     (loop while formatters
	   do (apply #'format out (pop formatters)
		     (loop while (and formatters (not (stringp (car formatters))))
			   collect (? data (pop formatters))))))))

(define-getx suppose (data &rest indicators)
  :query-lambda (data indicators)
  "Only proceed query if sub-query is true, otherwise abort query and
return NIL."
  (when (apply #'? data indicators)
    (proceed data)))

(define-getx pick (data this-indicator other-data-list-indicator &optional (other-indicator this-indicator) (test 'equal))
  :query-lambda (data this-indicator other-data-list-indicator other-indicator test)
  "First apply THIS-INDICATOR, then pick out first element of
OTHER-DATA-LIST whose OTHER-INDICATOR matches DATA under TEST. Proceed
query with that (other) element."
  (let ((this-data (? data this-indicator)))
    ;; (warn "this-data: ~S # ~S-> ~S"  data this-indicator this-data)
    (loop for other-data in (? data other-data-list-indicator)
	  when (funcall test this-data (? other-data other-indicator))
	    return (proceed other-data))))

(define-getx join (list $indicator $other-list &optional ($other-indicator $indicator) (test 'equal) no-matches-p no-other-matches-p (other-first-p t))
  :query-lambda (list $indicator $other-list $other-indicator test no-matches-p no-other-matches-p other-first-p)
  "For each element of LIST, append first entry from $OTHER-LIST where
LIST:$INDICATOR matches $OTHER-LIST:$OTHER-INDICATOR under
TEST. Optionally include entries where either list doesn't match. If
OTHER-FIRST-P is true (default true), make entries from $OTHER-LIST
first in the results."
  (proceed
   (loop with other-list = (? list $other-list)
	 for element in list
	 nconc (multiple-value-bind (value ok?)
		   (? element $indicator)
		 (when (or ok? no-matches-p)
		   (let ((other-element
			   (? other-list (seek $other-indicator value test))))
		     (when (or other-element no-other-matches-p)
		       (list (if other-first-p
			   (append other-element element)
			   (append element other-element))))))))))

(define-getx seq (sequence &rest indicators)
  :query-lambda (sequence indicators)
  "For each element of SEQUENCE apply INDICATORS (implicit PROGN?) and
proceed with a list of the results."
  (proceed
   (etypecase sequence
     (list
      (loop for element in sequence
	    collect (apply #'? element indicators)))
     (vector
      (loop for element across sequence
	    collect (apply #'? element indicators))))))

(define-getx cat (sequence &rest indicators)
  :query-lambda (sequence indicators)
  "For each element of SEQUENCE apply INDICATORS (implicit PROGN?) and
proceed with a concatenation of the (list) results."
  (proceed
   (etypecase sequence
     (list
      (loop for element in sequence
	    append (apply #'? element indicators)))
     (vector
      (loop for element across sequence
	    append (apply #'? element indicators))))))

#+ignore
(define-getx default (data $indicator &optional $default)
  :query-lambda (data $indicator $default)
  "If query for INDICATOR executes normally, proceed with that
value (even if NIL). If query is aborted (i.e. by missing key),
proceed query with DEFAULT value instead."
  (let ((good-value '#:good-value))
    (multiple-value-bind (value good-probe)
	(?? data $indicator (getx:call #'values good-value))
      (if (eq good-probe good-value)
	  (proceed value)
	  (proceed (? data $default))))))

(define-getx default (data &rest $indicators)
  :query-lambda (data $indicators)
  (loop for indicator in $indicators
	do (multiple-value-bind (value goodness-probe)
	       (? data indicator)
	     (when goodness-probe
	       (return (proceed value))))))

(define-getx else (data $else)
  "If DATA is false, proceed query with $ELSE instead."
  (proceed (or data (? data $else))))

(define-getx else! (data $else)
  "If DATA is false, terminate query with $ELSE instead."
  (if data
      (proceed data)
      (? data $else)))

(define-getx orelse (data)
  "Terminate query if DATA is true, otherwise proceed."
  (or data (proceed data)))

(define-getx thereis (list &rest $sub-query)
  :query-lambda (list $sub-query)
  "Proceed query with the result of the first element of LIST
for which SUB-QUERY is non-NIL."
  (loop for element in list
	for subq = (apply #'? element $sub-query)
	when subq
	  return (proceed element)))

(define-getx bind* (data &rest bindings &key &allow-other-keys)
  :query-lambda (data bindings)
  (loop for (var indicator) on bindings by #'cddr
	collect var into vars
	collect (? data indicator) into vals
	finally
	   (return (progv vars vals (proceed data)))))

(defmacro with-bindings ((&rest bindings) &body implicit-prog?)
  `(symbol-macrolet ,(loop for (var) in bindings collect `(,var (var* ',var)))
     (declare (inline bind* var*))
     (progn?
      (bind* ,@(loop for (var val) in bindings collect `(quote ,var) collect val))
      ,@implicit-prog?)))

(define-getx var* (data variable &rest implicit-prog?)
  :query-lambda (data variable implicit-prog?)
  (declare (ignore data))
  (proceed (apply #'? (symbol-value variable) implicit-prog?)))


(define-getx combine* (data function &rest $sub-queries)
  :query-lambda (data function $sub-queries)
  "Collect the results of SUB-QUERIES and combine the results like CL:REDUCE."
  (proceed
   (reduce function
	   (mapcar (lambda (sub-query)
		     (? data sub-query))
		   $sub-queries))))

(define-getx combine (list function &rest $sub-queries)
  :query-lambda (list function $sub-queries)
  "Collect the results of SUB-QUERIES over LIST and combine the results like CL:REDUCE."
  (proceed
   (reduce function list
	   :key (lambda (x)
		  (apply #'? x $sub-queries)))))

(define-getx affirm (data &rest $implicit-prog?)
  :query-lambda (data $implicit-prog?)
  "Assert $IMPLICIT-PROG? is true, or err."
  (unless (apply #'? data $implicit-prog?)
    (error "Affirmation failed on ~S." data))
  (proceed data))

(define-getx is (data value &optional (test #'equal))
  :query-lambda (data value test)
  "Terminate with NIL unless DATA is VALUE under TEST."
  (when (funcall test data value)
    (proceed data)))

(define-getx also (data &rest $sub-queries)
  :query-lambda (data $sub-queries)
  "If every SUB-QUERY is true, proceed with DATA."
  (when (loop for sq in $sub-queries
	      always (? data sq))
    (proceed data)))

(define-getx slice (sequence start &optional end)
  :query-lambda (sequence start end)
  "Slice a sequence, similar to Python slicing, i.e. negative index
counts from the end. Will try to share structure with SEQUENCE as much
as possible. An empty vector will return as NIL."
  (check-type start integer)
  (check-type end (or null integer))
  (proceed
   (etypecase sequence
     (list
      (if (eql end 0)
	  nil
	  (let* ((length nil)
		 (start-pos nil)
		 (slice-start
		   (etypecase start
		     ((eql 0)
		      (setf start-pos 0)
		      sequence)
		     ((integer 1 *)
		      (setf start-pos start)
		      (nthcdr start sequence))
		     ((integer * -1)
		      (setf start-pos 0)
		      (loop for slice = sequence then (cdr slice)
			    for runner = (loop initially (setf length 0)
					       #| this loop is (nthcdr (- start) sequence) with counting LENGTH |#
					       for x = sequence then (cdr x)
					       repeat (- start)
					       while x
					       do (incf length)
					       finally (return x))
			      then (cdr runner)
			    while runner
			    do (incf length)
			       (incf start-pos)
			    finally (return slice))))))
	    (when slice-start
	      (etypecase end
		(null
		 slice-start)
		((integer 1 *)
		 (loop for i upfrom start-pos below end
		       while slice-start
		       collect (pop slice-start)))
		((integer * -1)
		 (loop for i upfrom start-pos below (+ end (or length (length sequence)))
		       do (assert slice-start)
		       collect (pop slice-start))))))))
     (vector
      (let* ((length
	       (length sequence))
	     (end-pos
	       (cond
		 ((not end) length)
		 ((< end 0) (+ length end))
		 (t end))))
	(cond
	  ((= 0 length)
	   sequence)
	  ((< end-pos 0)
	   nil)
	  (t (etypecase start
	       ((eql 0)
		(if (>= end-pos length)
		    sequence
		    (subseq sequence 0 end-pos)))
	       ((integer 1 *)
		(if (>= start end-pos)
		    nil
		    (subseq sequence start end-pos)))
	       ((integer * -1)
		(let ((start-pos (- length start)))
		  (if (<= start-pos end-pos)
		      nil
		      (subseq sequence start-pos end-pos))))))))))))
