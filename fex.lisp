;;;; This is still not FEXPRs again
;;;
;;; I had a previous version of this which I seem to have lost
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.utilities :compile t))

(defpackage :org.tfeb.toys.fex
  (:use :cl :org.tfeb.hax.utilities)
  (:export
   ;; I am not sure about exposing this lower-level promisery
   #:delay #:force #:promisep #:forcedp
   #:ensure #:ensuring
   #:fex-boundp #:symbol-fex #:undefined-fex
   #:define-fex
   #:flet/fex
   #:labels/fex))

(in-package :org.tfeb.toys.fex)

(provide :org.tfeb.toys.fex)

;;;; Atomic promises
;;;
;;; The form may be evaluated several times if there is a race, but
;;; assuming that assignment to structure slots is atomic, things
;;; should remain consistent.
;;;

(defstruct (promise
            (:print-function (lambda (o s d)
                               (declare (ignore d))
                               (print-unreadable-object (o s :type t :identity t))))
            (:constructor make-promise (slot))
            (:predicate promisep))
  "A promise which can be forced"
  (slot (load-time-value (cons t nil))
        :type cons))

(defun force (p)
  "Force a promise, returning its value and computing it on the first call"
  (declare (type promise p))
  (let ((c (promise-slot p)))
    (if (car c)
        (cdr c)
      (let ((v (funcall (cdr c))))
        (setf (promise-slot p) (cons t v))
        v))))

(defun forcedp (p)
  "Has a promise been forced?"
  (declare (type promise p))
  (car (promise-slot p)))

(declaim (inline ensure))

(defun ensure (thing)
  "Ensure an object: if it is a promise force it, otherwise return it"
  (typecase thing
    (promise (force thing))
    (t thing)))

(defmacro ensuring (variables &body forms)
  `(let ,(mapcar (lambda (v)
                   `(,v (ensure ,v)))
                 variables)
     ,@forms))

(defmacro delay (&body forms)
  "Delay some forms, constructing a promise to evaluate them"
  `(make-promise (cons nil (lambda ()
                             ,@forms))))

(define-condition undefined-fex (cell-error)
  ()
  (:report (lambda (c s)
             (format s "Undefined fex ~S" (cell-error-name c))))
  (:documentation
   "The error raised when a fex is not defined"))

(define-condition simple-program-error (program-error simple-error)
  ())

(defun simple-program-error (control &rest arguments)
  (error 'simple-program-error
         :format-control control
         :format-arguments arguments))

(declaim (inline symbol-fex (setf symbol-fex)))

(defun fex-boundp (s)
  "Does a symbol name a fex?"
  (and (get s 'fex) t))

(defun symbol-fex (s)
  "Accessor for the fex value of a symbol"
  (let ((f (get s 'fex)))
    (unless f
      (error 'undefined-fex :name s))
    f))

(defun (setf symbol-fex) (f s)
  "Set the fex value of a symbol"
  (check-type f function)
  (setf (get s 'fex) f))

(unless (constantp ':org.tfeb.toys.fex)
  (warn "Doom: keywords are not constants"))

(defun expand-fex-call (to-call environment arguments)
  `(funcall ,to-call
            ,@(mapcar (lambda (a)
                        (if (constantp a environment)
                            a
                          `(delay ,a)))
                      arguments)))

(defmacro define-fex (name args &body doc/decls/forms)
  "Define a fex

A fex is like a function, but gets arguments which are not known to be
constant as promises.  It should use ensure to get values of its
arguments.  Fexes are implemented as macros which wrap arguments which
constantp can't tell are constant in delay forms."
  (multiple-value-bind (doc decls forms) (parse-docstring-body doc/decls/forms)
    `(progn
       (setf (symbol-fex ',name)
             (lambda ,args
               ,@decls
               (block ,name
                 ,@forms)))
       ,@(when doc
           `((setf (documentation ',name 'function) ,doc)))
       (defmacro ,name (&environment environment &rest arguments)
         (expand-fex-call `(symbol-fex ',',name) environment arguments)))))

;;; It is interesting that these two are identical except for
;;; inverting the order of binding
;;;

(defmacro flet/fex (bindings &body decls/forms)
  "Like FLET but for fexes"
  (let ((anons (mapcar (lambda (b)
                         (unless (and (listp b) (>= (length b) 3))
                           (simple-program-error "bad binding ~S" b))
                         (let ((n (first b)))
                           (unless (symbolp n)
                             (simple-program-error "fex name ~S not a symbol" n))
                           (make-symbol (symbol-name n))))
                       bindings)))
    `(flet ,(mapcar (lambda (a b)
                      `(,a ,@(rest b)))
                      anons bindings)
       (macrolet ,(mapcar (lambda (a b)
                            `(,(first b)
                              (&environment environment &rest arguments)
                              (expand-fex-call `(function ,',a) environment arguments)))
                          anons bindings)
         ,@decls/forms))))

#+(and LispWorks LW-Editor)
(editor:setup-indent "flet/fex" 1 nil nil 'flet)

(defmacro labels/fex (bindings &body decls/forms)
  "Like LABELS but for fexes"
  (let ((anons (mapcar (lambda (b)
                         (unless (and (listp b) (>= (length b) 3))
                           (simple-program-error "bad binding ~S" b))
                         (let ((n (first b)))
                           (unless (symbolp n)
                             (simple-program-error "fex name ~S not a symbol" n))
                           (make-symbol (symbol-name n))))
                       bindings)))
    `(macrolet ,(mapcar (lambda (a b)
                          `(,(first b)
                            (&environment environment &rest arguments)
                            (expand-fex-call `(function ,',a) environment arguments)))
                        anons bindings)
       (labels ,(mapcar (lambda (a b)
                          `(,a ,@(rest b)))
                        anons bindings)
         ,@decls/forms))))

#+(and LispWorks LW-Editor)
(editor:setup-indent "labels/fex" 1 nil nil 'flet)
