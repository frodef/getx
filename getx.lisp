
(in-package #:getx)

(defparameter *special-getx-operators* nil
  "Records a list of operators defined by DEFINE-GETX.")

(defun ? (x &rest indicators)
  "Query a hierarchical data-structure X by recursively applying
INDICATORS sequentially.

If INDICATOR is an atom (typically a keyword) it is either looked up
 in X by CL:GETF or CL:GETHASH, depending on the type of X.

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
    (t (apply #'?
	      (getx x (car indicators))
	      (cdr indicators)))))

(defmethod documentation ((x (eql '?)) (doc-type (eql 'function)))
  (format nil (documentation #'? 'function) *special-getx-operators*))

(defmacro define-getx (name getter-lambda &body body)
  (let ((getter-name (intern (format nil "%~A" name))))
    `(progn
       (pushnew ',name *special-getx-operators*)
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

(define-getx index (plist n)
  "Take the Nth element of PLIST."
  (proceed
   (etypecase plist
     (list
      (nth n plist))
     (vector
      (when (<= 0 n (length plist))
	(aref plist n))))))

(define-getx seek (plist indicator value &optional (test 'equal))
  "Find the first element of PLIST where INDICATOR matches VALUE under TEST."
  (dolist (sub-plist plist)
    (when (funcall test value (? sub-plist indicator))
      (return (proceed sub-plist)))))

(define-getx select (plists indicator value &optional (test 'equal))
  "Select each plist in PLISTS where INDICATOR matches VALUE under TEST. Returns a list of plists."
  (mapcan (lambda (plist)
	    (when (funcall test value (? plist indicator))
	      (list (proceed plist))))
	  plists))

(define-getx prog? (plist &rest indicators)
  "Process a sequence of INDICATORS."
  (proceed (apply #'? plist indicators)))

(define-getx multiple (plist &rest indicators)
  "Return a list from processing multiple INDICATORS."
  (mapcar (lambda (indicator)
	    (proceed (? plist indicator)))
	  indicators))

(define-getx each (plist each-key)
  "Return a list of all values of PLIST for KEY (as opposed to just
returning the first value for KEY)."
  (loop for (key value) on plist by #'cddr
	when (eq key each-key)
	  collect (proceed value)))

(define-getx each-key (plist)
  "Return the keys of PLIST, discarding the values."
  (etypecase plist
    (list
     (loop for (key value) on plist by #'cddr
	   collect (proceed key)))
    (hash-table
     (loop for key being the hash-keys of plist
	   collect key))))

(define-getx each-value (plist)
  "Return the values of PLIST, discarding the keys."
  (loop for (key value) on plist by #'cddr
	collect (proceed value)))

(define-getx either (plist &rest indicators)
  "Return the first INDICATOR that returns a non-NIL result."
  (loop for indicator in indicators
	thereis (proceed (? plist indicator))))

(define-getx slot (object slot-name)
  "Return the slot of an object."
  (proceed (slot-value object slot-name)))
