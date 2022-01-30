
(in-package #:getf)

(defvar *special-getf-operators* nil
  "Records a list of operators defined by DEFINE-GETF.")

(defun ? (plist &rest indicators)
  "Query a hierarchy of PLISTs (or list of PLISTs) by recursively
applying a INDICATORS sequentially. Each INDICATOR is an
atom (typically a keyword) tested under EQ against PLIST, or a special
indicator form that can also operate on a list of PLISTs.~@[ See the
documentation for the individual special indicators: ~{~S~^, ~}.~]

For example, to find the email address and phone number of every
employee in some Acme company whose first name is Frode:
  (getf:? company-plists (getf:seek :name \"Acme\" 'str:contains?) :employees (getf:select :first-name \"Frode\") (getf:multiple :email :phone))"
  (cond
    ((null indicators)
     plist)
    ((consp (car indicators))
     (apply (caar indicators)
	    (cdr indicators)
	    plist
	    (cdar indicators)))
    ((hash-table-p plist)
     (apply #'?
	    (gethash (car indicators) plist)
	    (cdr indicators)))
    (t (apply #'?
	      (getf plist (car indicators))
	      (cdr indicators)))))

(defmethod documentation ((x (eql '?)) (doc-type (eql 'function)))
  (format nil (documentation #'? 'function) *special-getf-operators*))

(defmacro define-getf (name getter-lambda &body body)
  (let ((getter-name (intern (format nil "%~A" name))))
    `(progn
       (pushnew ',name *special-getf-operators*)
       (defun ,getter-name (continuation ,@getter-lambda)
	 (flet ((proceed (new-plist)
		  (apply #'? new-plist continuation)))
	   ,@(if (stringp (first body))
		 (cdr body)
		 body)))
       (defun ,name (&rest args)
	 ,@(when (stringp (first body))
	     (list (concatenate 'string "Special indicator: " (first body))))
	 (list* ',getter-name args)))))

(define-getf index (plist n)
  "Take the Nth element of PLIST."
  (proceed
   (etypecase plist
     (list
      (nth n plist))
     (vector
      (when (<= 0 n (length plist))
	(aref plist n))))))

(define-getf seek (plist indicator value &optional (test 'equal))
  "Find the first element of PLIST where INDICATOR matches VALUE under TEST."
  (dolist (sub-plist plist)
    (when (funcall test value (? sub-plist indicator))
      (return (proceed sub-plist)))))

(define-getf select (plist indicator value &optional (test 'equal))
  "Select each element of PLIST where INDICATOR matches VALUE under TEST. Returns a list."
  (mapcan (lambda (sub-plist)
	    (when (funcall test value (? sub-plist indicator))
	      (list (proceed sub-plist))))
	  plist))

(define-getf prog? (plist &rest indicators)
  "Process a sequence of INDICATORS."
  (proceed (apply #'? plist indicators)))

(define-getf multiple (plist &rest indicators)
  "Return a list from processing multiple INDICATORS."
  (mapcar (lambda (indicator)
	    (proceed (? plist indicator)))
	  indicators))

(define-getf each (plist each-key)
  "Return a list of all values of PLIST for KEY (as opposed to just
returning the first value for KEY)."
  (loop for (key value) on plist by #'cddr
	when (eq key each-key)
	  collect (proceed value)))

(define-getf each-key (plist)
  "Return the keys of PLIST, discarding the values."
  (etypecase plist
    (list
     (loop for (key value) on plist by #'cddr
	   collect (proceed key)))
    (hash-table
     (loop for key being the hash-keys of plist
	   collect key))))

(define-getf each-value (plist)
  "Return the values of PLIST, discarding the keys."
  (loop for (key value) on plist by #'cddr
	collect (proceed value)))

(define-getf either (plist &rest indicators)
  "Return the first INDICATOR that returns a non-NIL result."
  (loop for indicator in indicators
	thereis (proceed (? plist indicator))))
