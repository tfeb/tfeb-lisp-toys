;;;; Fluids, finally
;;;

;;; The Python version of this (the name of which derived from fluid
;;; variables in, I think, Cambridge Lisp) had fluids as functions:
;;; calling the function with no arguments returned its value and with
;;; one argument set its value.  An earlier version of this did the
;;; same thing, but that's not really useful with a Lisp-2: you need
;;; things like FLUID-VALUE &c, and fluids-as-functions are likely a
;;; lot more heavyweight than just as a cons, which they now are.
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
   #:fluid-name
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

(defvar *fluid-bindings* '())

(defun make-fluid (&optional (name nil) (value nil valuep))
  ;; make a fluid with a name and an optional global value
  (if valuep
      (list name value)
    (cons name nil)))

(defun fluid-name (fluid)
  (car fluid))

(defun fluid-value (fluid &optional (toplevel nil))
  ;; Optional argument means to get the toplevel value
  (if (not toplevel)
      (let ((dynamic (assoc fluid *fluid-bindings*)))
        (if dynamic
            (cdr dynamic)
          (if (not (null (cdr fluid)))
              (cadr fluid)
            (error 'unbound-fluid-error :fluid fluid))))
    (if (not (null (cdr fluid)))
        (cadr fluid)
      (error 'unbound-fluid-error :fluid fluid))))

(defun (setf fluid-value) (new fluid &optional (toplevel nil))
  ;; Optional argument *sets* toplevel value (even if unbound)
  (if (not toplevel)
      (let ((dynamic (assoc fluid *fluid-bindings*)))
        (if dynamic
            (setf (cdr dynamic) new)
          (if (not (null (cdr fluid)))
              (setf (cadr fluid) new)
            (error 'unbound-fluid-error :fluid fluid))))
    (progn
      (setf (cdr fluid) (list new))
      new)))

(defun fluid-boundp (fluid &optional (toplevel nil))
  ;; normally return whether the fluid is bound and whether it is
  ;; toplevel-bound (note that T, NIL are not possible).  With
  ;; optional argument return toplevel bound and local bound, and note
  ;; all possibilites can happen in this case.
  (let ((locally (if (assoc fluid *fluid-bindings*) t nil))
        (globally (not (null (cdr fluid)))))
    (if (not toplevel)
        (values (or locally globally) globally)
      (values globally locally))))

(defun fluid-makunbound (fluid)
  ;; Make a fluid be unbound: this is legal only if there are no local
  ;; bindings of it.  See MAKUNBOUND and also
  ;; http://cl-su-ai.lisp.se/msg03748.html for discussion around it.
  ;; In summary: this is (probably) not the same as MAKUNBOUND (but I
  ;; think MAKUNBOUND should not do what it (probably) does).
  (when (assoc fluid *fluid-bindings*)
    (error 'bound-fluid-error :fluid fluid))
  (setf (cdr fluid) nil)
  fluid)

(defmacro define-fluid (name &optional (toplevel-value nil toplevel-value-p))
  ;; Should this be DEFPARAMETER?
  (if toplevel-value-p
      `(defvar ,name (make-fluid ',name ,toplevel-value))
    `(defvar ,name (make-fluid ',name))))

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
                  (acons (car nvt) (cadr nvt)
                         (make-bindings (cddr nvt))))))))
    (funcall thunk)))

(defmacro fluid-let ((&rest bindings) &body forms)
  ;; This could/should be written in terms of CALL/FLUID-BINDINGS but
  ;; isn't
  `(let ((*fluid-bindings*
          ,(iterate mb ((bt bindings))
             (if (null bt)
                 '*fluid-bindings*
               (destructuring-bind (binding . more) bt
                 (if (listp binding)
                     `(acons ,(first binding) ,(second binding)
                             ,(mb more))
                   `(acons ,binding nil
                           ,(mb more))))))))
     ,@forms))

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
