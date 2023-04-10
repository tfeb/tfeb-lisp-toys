;;;; Fake Global Lexical variables
;;; $Id$

(defpackage :org.tfeb.toys.glex
  (:use :cl)
  (:export
   #:defglex
   #:defglex*
   #:make-glex-readtable))

(in-package :org.tfeb.toys.glex)

(provide :org.tfeb.toys.glex)

;;; This makes the symbol-macro easier to write (want an error on unbound)
;;;
(declaim (inline glex-value (setf glex-value)))

(define-condition unbound-global-lexical (unbound-variable)
  ;; I feel guilty about using UNBOUND-VARIABLE because it isn't.
  ())

;;; This just uses property lists behind the scenes now
;;;

(declaim (inline glex-value (setf glex-value)))

(defun glex-value (sym)
  (check-type sym symbol "a symbol")
  (let* ((unbound (load-time-value (cons nil nil)))
         (val (get sym 'glex-value unbound)))
    (when (eq val unbound)
      (error 'unbound-global-lexical :name sym))
    val))

(defun (setf glex-value) (new sym)
  (check-type sym symbol "a symbol")
  (setf (get sym 'glex-value) new))

(defmacro defglex (x &optional (value nil valuep) (documentation nil docp))
  ;; Pronounced 'def-glex'
  "Define a global lexical variable.

Global lexicals are variables which exist globally but which don't
alter the semantics of local bindings.  DEFGLEX is like DEFVAR rather
than DEFPARAMETER: it will not modify existing bindings.

It won't work to define a global lexical which is already defined as a
global special variable: you'll get an error if you do so."
  `(progn
     ,@(if valuep
           `((unless (get-properties (symbol-plist ',x) '(glex-value))
               (setf (glex-value ',x) ,value)))
         '())
     (define-symbol-macro ,x (glex-value ',x))
     ,@(if docp
           `((setf (documentation ',x 'variable) ',documentation))
         '())
     ',x))

(defmacro defglex* (x value (documentation nil docp))
  ;; I wanted this to be DEFGLEP (pronounced 'def-glep'), but that
  ;; could be read as a predicate for DEFGLE-ness.
  "Define a global lexical parameter.

This is to DEFGLEX as DEFPARAMETER is to DEFVAR."
  `(progn
     (setf (glex-value ',x) ,value)
     (define-symbol-macro ,x (glex-value ',x))
     ,@(if docp
           `((setf (documentation ',x 'variable) ',documentation))
         '())
     ',x))

(define-condition glex-reader-error (reader-error simple-error)
  ())

(defun make-glex-readtable (&key (from *readtable*) (to nil)
                                 (dollar #\$))
  (let ((glrt (copy-readtable from to)))
    (when (get-dispatch-macro-character #\# dollar glrt)
      (error "Someone is already using #~A" dollar))
    (set-dispatch-macro-character
     #\# dollar
     (lambda (stream char n)
       (declare (ignore char n))
       (if (not *read-suppress*)
           (let ((got (read stream t nil t)))
             (unless (symbolp got)
               (error 'glex-reader-error
                      :format-control "global lexicals are symbols: ~S is ~S"
                      :format-arguments (list got (type-of got))
                      :stream stream))
             `(glex-value ',got))
           (read stream t nil t)))
     glrt)
    glrt))
