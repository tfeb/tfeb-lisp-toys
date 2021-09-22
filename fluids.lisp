;;;; Fluids, finally
;;;

;;; The Python version of this (the name of which derived from fluid
;;; variables in, I think, Cambridge Lisp) had fluids as functions:
;;; calling the function with no arguments returned its value and with
;;; one argument set its value.  An earlier version of this did the
;;; same thing, but that's not really useful with a Lisp-2: you need
;;; things like FLUID-VALUE &c, and fluids-as-functions are unlikely
;;; to be lighter-weight than representing them as CLOS objects with a
;;; single slot (or as conses, which they formerly were).  Symbols
;;; would be another option (use SYMBOL-VALUE for the toplevel
;;; binding), but I suspect they have more backage as well.  For
;;; either of these options you don't get FLUIDP, which I wanted.
;;;
;;; The Python version also had fluids which got new toplevel bindings
;;; per thread, which is easy in Python because it's a
;;; single-implementation language, so this version doesn't have that.
;;; That makes it possible to store the toplevel value in the fluid
;;; itself, which is nice, I think (no table of toplevel values).
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs (:org.tfeb.hax.iterate))

(defpackage :org.tfeb.toys.fluids
  (:use :cl :org.tfeb.hax.iterate)
  (:export
   #:fluid-error
   #:fluid-error-fluid
   #:unbound-fluid-error
   #:bound-fluid-error
   #:make-fluid
   #:fluidp
   #:fluid-value
   #:fluid-boundp
   #:fluid-makunbound
   #:define-fluid
   #:call/fluid-bindings
   #:fluid-let
   #:fluid-let*))

(in-package :org.tfeb.toys.fluids)

(provide :org.tfeb.toys.fluids)

(define-condition fluid-error (error)
  ((fluid :initarg :fluid :reader fluid-error-fluid)))

(define-condition unbound-fluid-error (fluid-error)
  ()
  (:report "unbound fluid"))

(define-condition bound-fluid-error (fluid-error)
  ()
  (:report "locally bound fluid"))

;;;; The FLUID class and its interfaces
;;; The interface is: the FLUID class with :TOPLEVEL-VALUE initarg &
;;; FLUID-TOPLEVEL-VALUE accessor, FLUID-BINDING function,
;;; REMOVE-FLUID-TOPLEVEL-VALUE function, and the binding stack
;;; *FLUID-BINDINGS*.
;;;

(defvar *fluid-bindings* '())

(defclass fluid ()
  ((toplevel-value :initarg :toplevel-value
                   :accessor fluid-toplevel-value)))

(defgeneric fluid-binding (fluid)
  ;; Return the local binding (a mutable cons) and whether there is a
  ;; global binding
  (:method ((fluid fluid))
   (values (assoc fluid *fluid-bindings*)
           (slot-boundp fluid 'toplevel-value))))

(defgeneric remove-fluid-toplevel-value (fluid)
  ;; Remove the toplevel value of a fluid
  (:method ((fluid fluid))
   (slot-makunbound fluid 'toplevel-value)))

(defvar *printing-fluids* nil)

(defmethod print-object ((fluid fluid) to)
  ;; Note this, intentionally, only uses the public interface to fluids
  (print-unreadable-object (fluid to :type t :identity t)
    (multiple-value-bind (tbp lbp) (fluid-boundp fluid t)
      (cond ((not (or tbp lbp))
             (write-string "unbound" to))
            ((member fluid *printing-fluids*)
             (write-string "unspeakable" to))
            (t
             (let ((*printing-fluids* (cons fluid *printing-fluids*)))
               (format to "~:[~*~;binding ~A~]~@[ ~*~]~:[~*~;toplevel ~A~]"
                       lbp (and lbp (fluid-value fluid))
                       lbp
                       tbp (and tbp (fluid-value fluid t)))))))))

;;;; Public interfaces built on the above
;;;

(defun make-fluid (&key (value nil valuep)
                        (from nil fromp))
  (if (or valuep (and fromp (fluid-boundp from t)))
      (make-instance 'fluid :toplevel-value (if valuep
                                                value
                                              (fluid-value from t)))
    (make-instance 'fluid)))

(defun fluidp (f)
  (typep f 'fluid))

(defun fluid-value (fluid &optional toplevel)
  ;; Optional argument means to get the toplevel value
  (multiple-value-bind (binding tbp) (fluid-binding fluid)
    (cond
     ((not toplevel)
      (cond
       (binding
        (cdr binding))
       (tbp
        (fluid-toplevel-value fluid))
       (t
        (error 'unbound-fluid-error :fluid fluid))))
     (tbp
      (fluid-toplevel-value fluid))
     (t
      (error 'unbound-fluid-error :fluid fluid)))))

(defun (setf fluid-value) (new fluid &optional (toplevel nil))
  ;; Optional argument sets toplevel value, even if currently unbound
  (if (not toplevel)
      (multiple-value-bind (binding tbp) (fluid-binding fluid)
        (cond (binding
               (setf (cdr binding) new))
              (tbp
               (setf (fluid-toplevel-value fluid) new))
              (t
               (error 'unbound-fluid-error :fluid fluid))))
    (setf (fluid-toplevel-value fluid) new)))

(defun fluid-boundp (fluid &optional (toplevel nil))
  ;; Normally return whether the fluid is bound and whether it is
  ;; toplevel-bound (note that T, NIL are not possible).  With
  ;; optional argument return toplevel bound and local bound, and note
  ;; all possibilites can happen in this case.
  (multiple-value-bind (binding tbp) (fluid-binding fluid)
    (if (not toplevel)
        (values (or (if binding t nil) tbp) tbp)
      (values tbp (if binding t nil)))))

(defun fluid-makunbound (fluid)
  ;; Make a fluid be unbound: this is legal only if there are no local
  ;; bindings of it.  See MAKUNBOUND and also
  ;; http://cl-su-ai.lisp.se/msg03748.html for discussion around it.
  ;; In summary: this is (probably) not the same as MAKUNBOUND (but I
  ;; think MAKUNBOUND should not do what it (probably) does).
  (multiple-value-bind (binding tbp) (fluid-binding fluid)
    (when binding
      (error 'bound-fluid-error :fluid fluid))
    (when tbp
      (remove-fluid-toplevel-value fluid))
    fluid))

(defmacro define-fluid (name &optional (toplevel-value nil toplevel-value-p)
                             documentation)
  ;; Deals with the simple case
  `(progn
     ,(if toplevel-value-p
          `(defvar ,name (make-fluid :toplevel-value ,toplevel-value))
        `(defvar ,name (make-fluid)))
     (setf (documentation ',name 'variable) ,documentation)
     ',name))

(defun call/fluid-bindings (thunk &rest names/values)
  ;; Call THUNK with fluids bound
  (declare (dynamic-extent names/values))
  (let ((*fluid-bindings*
         (iterate make-bindings ((nvt names/values))
           (cond ((null nvt)
                  *fluid-bindings*)
                 ((null (cdr nvt))
                  (error "odd number of names/values"))
                 (t
                  (destructuring-bind (f v . more) nvt
                    (unless (fluidp f)
                      (error 'simple-type-error
                             :format-control "~A is not a fluid"
                             :format-arguments (list f)
                             :datum f
                             :expected-type 'fluid))
                    (acons f v
                           (make-bindings more))))))))
    (funcall thunk)))

(defmacro fluid-let ((&rest bindings) &body forms)
  ;; This used not to be written in terms of CALL/FLUID-BINDINGS but
  ;; it should always have been and the type-checking makes it
  ;; compellingly easier now.
  `(call/fluid-bindings
    (lambda ()
      ,@forms)
    ,@(mapcan (lambda (b)
                (if (listp b)
                    b
                  (list b)))
              bindings)))

(defmacro fluid-let* ((&rest bindings) &body forms)
  ;; Sequential
  (cond ((null bindings)
         `(progn ,@forms))
        ((null (rest bindings))
         `(fluid-let ,bindings ,@forms))
        (t
         `(fluid-let ,(list (first bindings))
            (fluid-let* ,(rest bindings)
              ,@forms)))))
