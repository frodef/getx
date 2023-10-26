
(defpackage #:getx
  (:use
   #:common-lisp)
  (:export
   ? ?? p?
   *standard-object-plist*
   all*
   associate
   call
   call*
   foreach*
   each-key*
   each-value*
   except
   either
   filter
   keep*
   keep-type*
   multiple
   multiple*
   multiple-values
   progn?
   seek
   select
   select*
   unselect*
   yield
   fmt
   suppose
   join
   pick
   seq
   cat
   default))
