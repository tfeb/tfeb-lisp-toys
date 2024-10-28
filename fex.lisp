;;;; This is still not FEXPRs again
;;;
;;; I had a previous version of this which I seem to have lost
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ((:org.tfeb.hax.utilities
   :org.tfeb.hax.spam
   :org.tfeb.hax.collecting)
  :compile t))

(defpackage :org.tfeb.toys.fex
  (:use :cl :org.tfeb.hax.utilities :org.tfeb.hax.spam :org.tfeb.hax.collecting)
  (:export
   ;; I was not sure about exposing this lower-level promisery, but I
   ;; think it's nice enough
   #:delay #:force #:promisep #:forcedp #:promise-source-form
   #:ensure #:ensuring
   ;; same: this is not fexes (and it not used otherwise) but it's
   ;; nice enough.  Should there be all the options?
   #:let/lazy #:let*/lazy
   #:let/once #:let*/once
   #:let/lazy/once #:let*/lazy/once
   #:fex-boundp #:symbol-fex #:undefined-fex
   #:define-fex
   #:flet/fex
   #:labels/fex
   #:funcall/fex))

(in-package :org.tfeb.toys.fex)

(provide :org.tfeb.toys.fex)

;;;; Atomic promises
;;;
;;; The forms may be evaluated several times if there is a race, but
;;; assuming that assignment to structure slots is atomic, things
;;; should remain consistent.
;;;

(defstruct (promise
            (:print-function (lambda (o s d)
                               (declare (ignore d))
                               (print-unreadable-object (o s :identity t)
                                 ;; Pretty random choices but seem reasonable
                                 (let ((*print-length* 3)
                                       (*print-level* 4))
                                   (format s "promise of ~S"
                                           (promise-source-form o))))))
            (:constructor make-promise (slot source-form))
            (:predicate promisep))
  "A promise which can be forced"
  (slot (load-time-value (cons t nil))
        :type cons)
  (source-form nil :read-only t))

(defun force (p)
  "Force a promise, returning its value and computing it on the first call"
  (declare (type promise p))
  (let ((c (promise-slot p)))
    (if (car c)
        (cdr c)
      (let ((v (funcall (cdr c))))
        (setf (promise-slot p) (cons t v))
        v))))

(defun (setf force) (v p)
  ;; This is really questionable, but it makes let/lazy be cooler
  (declare (type promise p))
  (let ((c (promise-slot p)))
    (if (car c)
        (setf (cdr c) v)
      (progn
        (setf (promise-slot p) (cons t v))
        v))))

(defun forcedp (p)
  "Has a promise been forced?"
  (declare (type promise p))
  (car (promise-slot p)))

(define-condition simple-program-error (program-error simple-error)
  ())

(defun simple-program-error (control &rest arguments)
  (error 'simple-program-error
         :format-control control
         :format-arguments arguments))

(declaim (inline ensure))

(defun ensure (thing)
  "Ensure an object: if it is a promise force it, otherwise return it"
  (typecase thing
    (promise (force thing))
    (t thing)))

(defmacro ensuring (bindings &body forms)
  `(let ,(mapcar (lambda (b)
                   (matching b
                     ((is-type 'symbol)
                      `(,b (ensure ,b)))
                     ((list-matches (is-type 'symbol)) ;not sure this should be OK
                      `(,(first b) (ensure ,(first b))))
                     ((list-matches (is-type 'symbol) (any))
                      `(,(first b) (ensure ,(second b))))
                     (otherwise
                      (simple-program-error "bad ensuring binding ~S" b))))
                 bindings)
     ,@forms))

(defmacro delay (&body forms)
  "Delay some forms, constructing a promise to evaluate them"
  `(make-promise (cons nil (lambda ()
                             ,@forms))
                 ',(if (= (length forms) 1)
                       (first forms)
                     `(progn ,@forms))))

(defmacro let/lazy ((&rest bindings) &body decls/forms)
  "A lazy version of LET

For each let-style binding, this evaluates the initform exactly once,
at the time the binding is first referred to.  Later uses of the
variable return the cached value.  If the binding is never referred to
then the initform is never evaluated.

The 'variables' this binds can be assigned to: if you assign to such a
variable before using its value the initform will never be evaluated.
The 'variables' are really symbol macros so type declarations probably
don't work the way you might expect."
  (multiple-value-bind (names initforms secret-names)
      (with-collectors (name initform secret-name)
        (dolist (b bindings)
          (matching b
            ((is-type 'symbol)
             (name b)
             (initform nil)
             (secret-name (make-symbol (string b))))
            ((list-matches (is-type 'symbol))
             (name (first b))
             (initform nil)
             (secret-name (make-symbol (string (first b)))))
            ((list-matches (is-type 'symbol) (any))
             (name (first b))
             (initform (second b))
             (secret-name (make-symbol (string (first b)))))
            (otherwise
             (simple-program-error "bad binding ~S" b)))))
    `(let ,(mapcar (lambda (s i)
                     `(,s (delay ,i)))
                   secret-names initforms)
       (symbol-macrolet ,(mapcar (lambda (n s)
                                   `(,n (force ,s)))
                                 names secret-names)
         ,@decls/forms))))

(defmacro let*/lazy ((&rest bindings) &body decls/forms)
  "A lazy version of LET*

See LET/LAZY"
  (case (length bindings)
    (0
     `(locally decls/forms))
    (1
     `(let/lazy ,bindings ,@decls/forms))
    (otherwise
     `(let/lazy (,(first bindings))
        (let*/lazy ,(rest bindings) ,@decls/forms)))))

(defmacro let/once ((&rest bindings) &body decls/forms)
  "A cached version of LET

This is like LET but, in compiled code, the initforms will only ever
be evaluated only once.

The 'variables' bound here are symbol macros, so declarations may not
work the right way."
  (multiple-value-bind (names initforms secret-names)
      (with-collectors (name initform secret-name)
        (dolist (b bindings)
          (matching b
            ((is-type 'symbol)
             (name b)
             (initform nil)
             (secret-name (make-symbol (string b))))
            ((list-matches (is-type 'symbol))
             (name (first b))
             (initform nil)
             (secret-name (make-symbol (string (first b)))))
            ((list-matches (is-type 'symbol) (any))
             (name (first b))
             (initform (second b))
             (secret-name (make-symbol (string (first b)))))
            (otherwise
             (simple-program-error "bad binding ~S" b)))))
    `(let ,(mapcar (lambda (s)
                     `(,s (load-time-value (cons nil nil) nil)))
                   secret-names)
       ,@(mapcar (lambda (s i)
                   `(unless (car ,s)
                      (setf (cdr ,s) ,i
                            (car ,s) t)))
                 secret-names initforms)
       (symbol-macrolet ,(mapcar (lambda (n s)
                                   `(,n (cdr ,s)))
                                 names secret-names)
         ,@decls/forms))))

(defmacro let*/once ((&rest bindings) &body decls/forms)
  "A cached version of LET*

See LET/ONCE"
  (case (length bindings)
    (0
     `(locally decls/forms))
    (1
     `(let/once ,bindings ,@decls/forms))
    (otherwise
     `(let/once (,(first bindings))
        (let*/once ,(rest bindings) ,@decls/forms)))))

(defmacro let/lazy/once ((&rest bindings) &body decls/forms)
  "A lazy & cached version of LET

This is like LET/LAZY but, in compiled code, the initforms will
only ever be evaluated once."
  (multiple-value-bind (names initforms secret-names)
      (with-collectors (name initform secret-name)
        (dolist (b bindings)
          (matching b
            ((is-type 'symbol)
             (name b)
             (initform nil)
             (secret-name (make-symbol (string b))))
            ((list-matches (is-type 'symbol))
             (name (first b))
             (initform nil)
             (secret-name (make-symbol (string (first b)))))
            ((list-matches (is-type 'symbol) (any))
             (name (first b))
             (initform (second b))
             (secret-name (make-symbol (string (first b)))))
            (otherwise
             (simple-program-error "bad binding ~S" b)))))
    `(let ,(mapcar (lambda (s)
                     `(,s (load-time-value (cons nil nil) nil)))
                   secret-names)
       ,@(mapcar (lambda (s i)
                   `(unless (car ,s)
                      (setf (cdr ,s) (delay ,i)
                            (car ,s) t)))
                 secret-names initforms)
       (symbol-macrolet ,(mapcar (lambda (n s)
                                   `(,n (force (cdr ,s))))
                                 names secret-names)
         ,@decls/forms))))

(defmacro let*/lazy/once ((&rest bindings) &body decls/forms)
  "A lazy & cached version of LET*

See LET/LAZY/ONCE and also LET/LAZY & LET*/LAZY"
  (case (length bindings)
    (0
     `(locally decls/forms))
    (1
     `(let/lazy/once ,bindings ,@decls/forms))
    (otherwise
     `(let/lazy/once (,(first bindings))
        (let*/lazy/once ,(rest bindings) ,@decls/forms)))))

(define-condition undefined-fex (cell-error)
  ()
  (:report (lambda (c s)
             (format s "Undefined fex ~S" (cell-error-name c))))
  (:documentation
   "The error raised when a fex is not defined"))

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

(defmacro funcall/fex (&environment environment f &rest arguments)
  (expand-fex-call f environment arguments))
