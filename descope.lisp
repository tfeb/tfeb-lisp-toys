;;;; Descoped variables
;;;

(defpackage :org.tfeb.toys.descope
  (:use :cl)
  (:export
   #:descoping
   #:descoped-variable
   #:descoped-variable-warning
   #:descoped-variable-error))

(in-package :org.tfeb.toys.descope)

(provide :org.tfeb.toys.descope)

(define-condition descoped-variable ()
  ())

(define-condition descoped-variable-warning (descoped-variable simple-warning)
  ((name :initarg :name :reader descoped-warning-name))
  (:documentation "Warning signaled for descoped varibles"))

(define-condition descoped-variable-error (descoped-variable unbound-variable)
  ((format-control :initarg :format-control
                   :initform "~S is descoped."
                   :reader descoped-variable-error-format-control))
  (:report (lambda (c s)
             (format s
                     (descoped-variable-error-format-control c)
                     (cell-error-name c))))
  (:documentation "Error signaled for descoped variables"))

(defmacro descoping ((&rest variables) &body forms)
  ;; This used to be called DESCOPE which was shorter and prettier but
  ;; sounded like a function not a macro.  DESCOPEING is
  ;; unpronounceable, so this.
  "Descope one or more variables

Within the body of this form a reference to one of the named variables
will cause a compile-time warning (a macro-expansion-time warning, in
face) and a run-time error."
  (assert (every #'symbolp variables) (variables)
    "Some variables aren't: ~{~S~^, ~}" (remove-if #'symbolp variables))
  `(symbol-macrolet ,(mapcar (lambda (v)
                               `(,v (descoped ,v)))
                             variables)
     (declare (ignorable ,@variables))
     ,@forms))

;;; Using macros is easier than relying on compiler-macros to get
;;; warnings at compile time
;;;
(defmacro descoped (variable)
  (warn 'descoped-variable-warning
        :name variable
        :format-control "~S is descoped."
        :format-arguments (list variable))
  `(error 'descoped-variable-error
          :name ',variable))

(defmacro set-descoped (variable new)
  (declare (ignore new))
  (warn 'descoped-variable-warning
        :name variable
        :format-control "assignment to ~S which is descoped."
        :format-arguments (list variable))
  `(error 'descoped-variable-error
          :format-control "assignment to ~S which is descoped."
          :name ',variable))

(defsetf descoped set-descoped)
