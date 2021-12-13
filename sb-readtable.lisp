;;;; Square Bracket readtables
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

(defun make-sb-readtable (&optional (from-readtable *readtable*)
                                   (to-readtable nil))
  "Make a readtable where [...] reads as [op ...], op is by default FUNCALL

FROM-READTABLE is the readtable to copy, defaultly *READTABLE*,
TO-READTABLE is the readtable to copy into, defaultly NIL, meanding
make a new readtable.

Return the readtable created.  If the existing readtable defines #\\[
or #\\] as in any way special an error is signalled.

*SB-OPERATOR-NAME* defines what op is."
  (let ((sbrt (copy-readtable from-readtable to-readtable)))
    (when (or (get-macro-character #\[ sbrt)
              (get-macro-character #\] sbrt))
      (error "readtable already defines [ and/or ] as a macro character"))
    (set-macro-character
     #\[
     (lambda (stream char)
       (declare (ignore char))
       (let ((form (read-delimited-list #\] stream t)))
         (if *sb-transformer*
             (funcall *sb-transformer* form stream)
           (cons *sb-operator-name* form))))
     nil sbrt)
    (set-macro-character
     #\]
     (lambda (stream char)
       (declare (ignore char))
       (error 'simple-reader-error
              :stream stream
              :format-control "unexpected right bracket"))
     nil sbrt)
    sbrt))
