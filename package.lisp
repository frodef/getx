
(defpackage #:getx
  (:use
   #:common-lisp)
  (:export
   ? ?? p?
   *standard-object-plist*
   all
   associate
   call
   call*
   fork
   each-key
   each-value
   except
   either
   filter
   keep
   listing
   listing*
   multiple-values
   progn?
   seek
   select
   unselect
   thereis
   yield
   fmt
   suppose
   join
   pick
   seq
   cat
   default
   else
   orelse
   with-bindings))
