;;;; This is not really locatives
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.collecting)

(defpackage :org.tfeb.toys.locatives
  (:use :cl :org.tfeb.hax.collecting)
  (:export
   #:locative
   #:with-locatives
   #:locf))

(in-package :org.tfeb.toys.locatives)

(provide :org.tfeb.toys.locatives)

(defmacro locative (place)
  "Return a locative for PLACE

A locative is something which can be used with LOCF and (SETF LOCF) to
access the value of PLACE.  Locatives are named after something that
existed on Lisp machines, but they are not really the same thing.

So, given

  (defun foo (loc)
    (setf (locf loc) 4))

then

  (let ((c 3))
    (foo (locative c))
    c)

will evaluate to 4."
  (let ((<value> (make-symbol "VALUE"))
        (<valuep> (make-symbol "VALUEP")))
  `(lambda (&optional (,<value> nil ,<valuep>))
     (if ,<valuep>
         (setf ,place ,<value>)
       ,place))))

(declaim (inline locf (setf locf)))

(defun locf (locative)
  "Retrieve the value of a locative"
  (funcall locative))

(defun (setf locf) (value locative)
  "Set the value of a locative"
  (funcall locative value))

(defmacro with-locatives ((&rest bindings) &body forms)
  "Bind symbol macros for locatives to make them look like variables

Each BINDING is either a symbol, which should be bound to a locative,
or a list of the form (<name> <form>), where <form> should evaluate to
a locative to which <name> will refer.

Example:

  (defun foo (loc)
    (with-locatives ((l loc))
      (setq l 4)))

Return the values of the last form in the body."
  (multiple-value-bind (let-bindings symbol-macro-bindings)
      (with-collectors (let-binding symbol-macro-binding)
        (dolist (b bindings)
          (typecase b
            (symbol (symbol-macro-binding `(,b (locf ,b))))
            (list
             (unless (and (symbolp (first b))
                          (= (length b) 2))
               (error "bad locative binding ~S" b))
             (destructuring-bind (var form) b
               (let ((<v> (make-symbol (symbol-name var))))
                 (let-binding `(,<v> ,form))
                 (symbol-macro-binding `(,var (locf ,<v>))))))
            (t
              (error "mutant locative binding ~S")))))
    (if (not (null let-bindings))
        `(let ,let-bindings
           (symbol-macrolet ,symbol-macro-bindings
             ,@forms))
      `(symbol-macrolet ,symbol-macro-bindings
         ,@forms))))
