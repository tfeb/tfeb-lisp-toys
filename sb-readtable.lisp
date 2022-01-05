;;;; Square Bracket (or more general brackets) readtables
;;;

(defpackage :org.tfeb.toys.sb-readtable
  (:use :cl)
  (:export
   #:make-sb-readtable
   #:*sb-operator-name*
   #:*sb-transformer*))

(in-package :org.tfeb.toys.sb-readtable)

(provide :org.tfeb.toys.sb-readtable)

(define-condition simple-reader-error (reader-error simple-error)
  ())

(defvar *sb-operator-name* 'funcall
  "The name of the operator interposed by [...]

Normally [...] reads as (op ...), where op is *SB-OPERATOR-NAME*.
Note that this variable has effect at read time.

If *SB-TRANSFORMER* is non-NIL, then this is not used: instead [...]
is transformed by the function which it designates")

(defvar *sb-transformer* nil
  "A function to fransform [...]

If this is not NIL then it should designate a function which is used
to transform the form read by [...].  This function is called a read
time, and takes two arguments: the form read and the stream.  The
second argument is intended to help when signalling errors.")

(defun make-sb-readtable (&key (from-readtable *readtable*)
                               (to-readtable nil)
                               (brackets '(#\[ #\])))
  "Make a readtable where [...] (or specified brackets) reads as described below.

FROM-READTABLE is the readtable to copy, defaultly *READTABLE*,
TO-READTABLE is the readtable to copy into, defaultly NIL, meanding
make a new readtable.

BRACKETS, if given, should be a list of an open and close bracket
characters, with the default being (#\\[ #\\]).

Return the readtable created.  If the existing readtable defines the
open or close bracket character as in any way special an error
is signalled.

Below assumes the brackets are the defaults.

If *SB-TRANSFORMER* is NIL [...] reads as (<op> ...) where <op> is the
value of *SB-OPERATOR-NAME*, which is by default FUNCALL.

If *SB-TRANSFORMER* is not NIL, it should be a function designator for
a function of two arguments: the form read and the stream it was read
from.  This function is responsible for transforming whatever was
read however it wishes to."
  (let ((sbrt (copy-readtable from-readtable to-readtable)))
    (destructuring-bind (open close) brackets
      (when (or (get-macro-character open sbrt)
                (get-macro-character close sbrt))
        (error "readtable already defines ~S and/or ~S as macro characters"
               open close))
      (set-macro-character
       open
       (lambda (stream char)
         (declare (ignore char))
         (let ((form (read-delimited-list close stream t)))
           (if *sb-transformer*
               (funcall *sb-transformer* form stream)
             (cons *sb-operator-name* form))))
       nil sbrt)
      (set-macro-character
       close
       (lambda (stream char)
         (declare (ignore char))
         (error 'simple-reader-error
                :stream stream
                :format-control "unexpected ~A" close))
       nil sbrt)
      sbrt)))
