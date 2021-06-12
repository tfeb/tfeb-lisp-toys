;;;; Racket-style parameters
;;; $Id$

(defpackage :org.tfeb.toys.parameters
  (:use :cl)
  (:export
   #:define-parameter
   #:define-parameter*
   #:parameterize
   #:parameterize*))

(in-package :org.tfeb.toys.parameters)

(provide :org.tfeb.toys.parameters)

(defmacro define-parameter (name &optional (value nil valuep)
                                 (documentation nil documentationp))
  "Define a parameter with an optional initial velue

A parameter is a function of no arguments which wraps a dynamic
variable.  Calling the function will return the value of the dynamic
variable.  For a parameter X, (SETF X) will set the value of the
variable.  The PARAMETERIZE form allows you to dynamicallt bind the
values of parameters.

DEFINE-PARAMETER is like DEFVAR: the initial value is only used if the
parameter is currently unbound.  See DEFINE-PARAMETER* for the
DEFPARAMETER equivalent."
  (let ((pvn (or (get name 'parameter-variable-name)
                 (setf (get name 'parameter-variable-name)
                       (make-symbol (concatenate
                                     'string "*" (symbol-name name) "*"))))))
    `(progn
       (defvar ,pvn ,@(if valuep `(,value) '()))
       (declaim (inline ,name (setf ,name)))
       (defun ,name ()
         ,@(if documentationp `(,documentation) '())
         ,pvn)
       (defun (setf ,name) (new)
         ,@(if documentationp
               (list (format nil "setter for ~A, which see" name))
             '())
         (setf ,pvn new))
       ',name)))

(defmacro define-parameter* (name value &optional
                                  (documentation nil documentationp))
  "Define a parameter with an initial value

See DEFINE-PARAMETER.  DEFINE-PARAMETER* is like DEFPARAMETER to
DEFINE-PARAMETER's DEFVAR."
  (let ((pvn (or (get name 'parameter-variable-name)
                 (setf (get name 'parameter-variable-name)
                       (make-symbol (concatenate
                                     'string "*" (symbol-name name) "*"))))))
    `(progn
       (defparameter ,pvn ,value)
       (declaim (inline ,name (setf ,name)))
       (defun ,name ()
         ,@(if documentationp `(,documentation) '())
         ,pvn)
       (defun (setf ,name) (new)
         ,@(if documentationp
               (list (format nil "setter for ~A, which see" name))
             '())
         (setf ,pvn new))
       ',name)))

(defmacro parameterize (bindings &body decls/forms)
  "Dynamically bind a number of parameters

This is like LET but for parameters.  See PARAMETERIZE* for the LET*
equivalent."
  (expand-parameterize 'let bindings decls/forms))

(defmacro parameterize* (bindings &body decls/forms)
  "Dynamically bind a number of parameters

This is like LET* but for parameters.  See PARAMETERIZE for the LET
equivalent."
  (expand-parameterize 'let* bindings decls/forms))

(defun expand-parameterize (binder bindings decls/forms)
  (unless (and (listp bindings)
               (every (lambda (e)
                        (and (listp e)
                             (= (list-length e) 2)
                             (symbolp (car e))))
                      bindings))
    (error "horribly malformed bindings"))
  (unless (every (lambda (b)
                   (if (get (first b) 'parameter-variable-name)
                       t
                     (warn "~S isn't a parameter" (first b))))
                 bindings)
    (error "undefined parameter/s"))
  `(,binder ,(mapcar (lambda (b)
                   (list (get (first b) 'parameter-variable-name)
                         (second b)))
                 bindings)
     ,@decls/forms))
