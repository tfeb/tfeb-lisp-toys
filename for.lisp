;;;; Rather trivial iteration protocol
;;; $Id$

;;; Copyright 2004, 2022 by me, Tim Bradshaw, and may be used for any
;;; purpose whatsoever by anyone. It has no warranty whatsoever. I
;;; would appreciate acknowledgement if you use it in anger, and I
;;; would also very much appreciate any feedback or bug fixes.
;;;
;;; NOTE: this version is incompatible with the 2004 version of this
;;; code in several ways: NEXT is now required to return an iterator
;;; as its second value (often this is the iterator it was passed but
;;; it may not be), or the exhausted-iterator object.  Ranges have
;;; gone, ITER is ITERATOR, other things are different.
;;;
;;; This does not strive for performance: there is at least a generic
;;; function call on every iteration and often more than that.  It
;;; could also pretty clearly do better by special-casing fixnum /
;;; integer cases, which it does not do.
;;;
;;; Iterators can't return multiple values.  They potentially could
;;; but it would make things much more complicated.
;;;
;;; At various points iterator constructors have been cleverer than
;;; they are now, so for instance (iterator -4) would have known to
;;; count down.  That's no longer true, but I am not completely sure
;;; this is right.  Related to this there is a question of what should
;;; happen for iterators with bounds which are out of range.
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ((:org.tfeb.hax.collecting
   :org.tfeb.hax.iterate
   :org.tfeb.hax.simple-loops
   :org.tfeb.dsm)
  :compile t))

(defpackage :org.tfeb.toys.for
  (:use :cl)
  (:use
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate
   :org.tfeb.hax.simple-loops
   :org.tfeb.dsm)
  (:export
   #:iterator
   #:exhausted-iterator
   #:for
   #:next
   #:endlessly
   #:endlessly*
   #:append-iterators
   #:append-iterators*
   #:gather))

(in-package :org.tfeb.toys.for)

(provide :org.tfeb.toys.for)

;;; Iteration protocol
;;;

(defconstant exhausted-iterator 'exhausted-iterator
  ;; This is not NIL because NIL is a legitimate iterator.
  "The special exhausted iterator object")

;(declaim (inline iterator next))

(defgeneric iterator (x &key)
  (:documentation "Return an iterator for X

If X is mutable then the returned iterator may not respect alterations
to the structure of X after it is constructed.  Functions are their
own iterators, and in general this should be idempotent on objects
which are already iteratorss: (ITERATOR (ITERATOR x ...)) must return
an iterator which works in the same way as the one returned by the
inner call.  This allows FOR to call it without fear.")
  (:method (x &key)
   (error "No iterator defined for class ~S"
          (class-name (class-of x))))
  (:method ((f function) &key)
   f))

(defgeneric next (iterator)
  (:documentation "Step an iterator

An iterator should return the next item, and an iterator, or any value
and the special EXHAUSTED-ITERATOR object when the iterator is
exhausted.")
  (:method ((iterator function))
   ;; simple-iterators are functions: just call them with themselves
   ;; as an argument
   (funcall iterator iterator)))

;;; Generators?  Should we have them?  I don't think so.

(defmacro for (bindings &body body)
  ;; bindings is either (v form &optional result) or ((v1 form1
  ;; &optional r1) ...).  Constructs iterators for all the forms,
  ;; steps them in parallel until the first is exhausted, then returns
  ;; a value for each form.  All forms are stepped the same numnber of
  ;; times.  the variables are assigned to on each step unless the
  ;; iterator is exhausted, when they keep their old values (this may
  ;; or may not be the right semantics).
  ;;
  ;; I am not sure about the special single-variable case: it's
  ;; convenient to be able to say (for (v ...) ...) but is it worth
  ;; the irregularity?
  "Iteration-based looping macro"
  (multiple-value-bind (vars forms result-forms)
      (destructuring-match bindings
        ((var form &optional result-form)
         (:when (symbolp var))
         (values (list var) (list form) (list result-form)))
        (bindings-list
         (:when (listp bindings-list))
         (with-collectors (var form result-form)
           (dolist (binding bindings-list)
             (destructuring-match binding
               ((var form &optional result-form)
                (var var) (form form) (result-form result-form))
               (otherwise
                (error "bad binding ~S" binding))))))
        (otherwise
         (error "mutant bindings ~S" bindings)))
    (let ((iterators (mapcar (lambda (var)
                               (make-symbol (concatenate 'string
                                                         "I-" (symbol-name var))))
                             vars))
          (<stop> (make-symbol "STOP")))
      `(let ((,<stop> nil)
             ,@(mapcar (lambda (iterator form)
                         (destructuring-match form
                           ((op . _)
                            (:when (eq op 'iterator))
                            `(,iterator ,form))
                           (otherwise
                            `(,iterator (iterator ,form)))))
                       iterators forms))
         ;; This won't help, but it's true.
         (declare (dynamic-extent ,@iterators))
         (do ,(mapcar (lambda (var iterator)
                        `(,var (multiple-value-bind (value next-iterator)
                                       (next ,iterator)
                                     (if (eq next-iterator exhausted-iterator)
                                         (progn
                                           (setf ,<stop> t)
                                           value)
                                       (progn
                                         (setf ,iterator next-iterator)
                                         value)))
                               (multiple-value-bind (value next-iterator)
                                   (next ,iterator)
                                 (if (eq next-iterator exhausted-iterator)
                                     (progn
                                       (setf ,<stop> t)
                                       ,var)
                                       (progn
                                         (setf ,iterator next-iterator)
                                         value)))))
                          vars iterators)
             (,<stop> (values ,@result-forms))
           ,@body)))))

;;; Some useful iterators
;;;

(defun endlessly* (elements)
  "Return an iterator which iterates endlessly over its argument, a list"
  (when (null elements)
    (error "can't endlessly iterate no elements"))
  (looping* ((tail (rest elements))
             (c (list (first elements)))
             (ct c))
    (when (null tail)
      (setf (cdr ct) c)
      (return c))
    (values (rest tail)
            c
            (setf (cdr ct) (list (first tail))))))

(defun endlessly (&rest elements)
  "Return an iterator which iterates endlessly over its arguments"
  (declare (dynamic-extent elements))
  (endlessly* elements))

(defun append-iterators* (iterators)
  (when (null iterators)
    (error "appending no iterators"))
  (destructuring-bind (this . more) iterators
    (lambda (self)
      (multiple-value-bind (v ni) (next this)
        (if (eq ni exhausted-iterator)
            (if more
                (progn (setf this (first more)
                             more (rest more))
                  (funcall self self))
              (values nil exhausted-iterator))
          (progn
            (setf this ni)
            (values v self)))))))

(defun append-iterators (&rest iterators)
  (append-iterators* iterators))

(defmethod iterator ((l list) &key (step 1)
                     (start 0) (end 0 endp)
                     (cyclic nil))
  ;; In the simple case lists are their own iterators
  (declare (type (integer 0) start end step))
  (cond
   ((and endp (>= start end))
    nil)
   ((and (not cyclic) (= step 1))
    (subseq l start (and endp end)))
   ((and (not cyclic) (= step 0))
    (let ((lt (if (= start 0) l (nthcdr (1- start) l))))
      (if (null lt)
          nil
        (endlessly (first lt)))))
   (cyclic
    (let* ((sl (subseq l start (and endp end)))
          (lt sl))
      (if (null sl)
          nil
        (lambda (self)
          (values
           (prog1 (first lt)
             (setf lt (looping ((i step) (tail lt))
                         (when (or (= i 0) (null tail))
                           (return (or tail sl)))
                         (values (1- i) (or (rest tail) sl)))))
               self)))))
   (t
    (let ((lt (subseq l start (and endp end))))
      (if (null lt)
          nil
        (lambda (self)
          (if (not (null lt))
              (values
               (prog1 (first lt)
                 (setf lt (looping ((i step) (tail lt))
                            (when (or (= i 0) (null tail))
                              (return tail))
                            (values (1- i) (rest tail)))))
               self)
            (values nil exhausted-iterator))))))))

(defmethod next ((iterator list))
  (if (null iterator)
      (values nil exhausted-iterator)
    (values (first iterator) (rest iterator))))

(deftype vector-index ()
  `(integer 0 (,array-dimension-limit)))

(defmethod iterator ((v vector) &key (step 1)
                     (start 0)
                     (end (length v))
                     (cyclic nil))
  (declare (type vector-index start end step))
  (cond
   ((= start end)
    (iterator '()))
   ((= step 0)
    (endlessly (aref v start)))         ;mutating v will not work
   (cyclic
    (let ((s start)
          (interval (if (< start end)
                     (1+ (- end start))
                   (- end start 1))))
      (lambda (self)
        (values
         (prog1 (aref v s)
           ;; I don't want to keep step count which might get bigger
           ;; than a fixnum, hence this slightly horrid expression
           (setf s (+ start (mod (- (+ s step) start) interval))))
         self))))
   ((> step 0)
      (let ((s start))
        (lambda (self)
          (if (< s end)
              (values
               (prog1 (aref v s)
                 (incf s step))
               self)
            (values nil exhausted-iterator)))))
   (t                                   ;(< step 0)
    (let ((s start))
      (lambda (self)
        (if (>= s end)
            (values
             (prog1 (aref v s)
               (incf s step))
             self)
          (values nil exhausted-iterator)))))))

(defmethod iterator ((n real) &key
                     (step 1)
                     (start (coerce 0 (type-of n)) startp)
                     (cyclic nil)
                     (endless nil))
  (declare (type real step start))
  (cond
   ((= start n)
    (iterator '()))
   ((= step 0)
    (endlessly start))
   (endless
    ;; count *from* n unless start is given
    (let ((i (if startp start n)))
      (declare (type real i))
      (lambda (self)
        (values (prog1 i (incf i step)) self))))
   (cyclic
    (let ((s start)
          (interval (- n start)))
      (declare (type real s interval))
      (lambda (self)
        (values
         (prog1 s
           (setf s (+ start (mod (+ s step) interval))))
         self))))
   ((> step 0)
    (let ((s start))
      (declare (type real s))
      (lambda (self)
        (if (< s n)
            (values
             (prog1 s
               (incf s step))
             self)
          (values nil exhausted-iterator)))))
   (t                   ;(< step 0)
    (let ((s start))
      (declare (type real s))
      (lambda (self)
        (if (> s n)
            (values
             (prog1 s
               (incf s step))
             self)
          (values nil exhausted-iterator)))))))

;;; List comprehensions
;;;

(defmacro gather (form &body clauses)
  ;; Python-style list comprehension operator.  See examples below
  ;; Each clause is simply rewritten to a `single-special-argument' macro:
  ;; foo x ... -> (foo x ...), so you can put anything there, even your
  ;; own macros: (gather x dolist (x '(1 2 3 4))) works.
  (let ((<a> (make-symbol "A"))
        (<at> (make-symbol "AT")))
    `(let ((,<a> '())
           (,<at> '()))
       ,(iterate make-form ((ct clauses))
          (destructuring-match ct
            (()
             ;; done: collect form
             `(if ,<a>
                  (setf (cdr ,<at>) (list ,form)
                        ,<at> (cdr ,<at>))
                (setf ,<a> (list ,form)
                      ,<at> ,<a>)))
            (((op . args) . more)
             (:when (symbolp op))
             ;; (gather x ... (for ...) ...)
             `(,op ,@args ,(make-form more)))
            ((op args . more)
             (:when (symbolp op))
             ;; (gather x ... for ... ...)
             `(,op ,args ,(make-form more)))
            (otherwise
             (error "bad clause tail ~S from ~S" ct clauses))))
       ,<a>)))

#||
(defmacro gathering (form &body clauses)
  ;; Like GATHER but it doesn't gather anything.  I see no use to
  ;; this.
  (iterate make-form ((ct clauses))
    (destructuring-match ct
      (()
       ;; Done: expansion is FORM
       form)
      (((op . args) . more)
       (:when (symbolp op))
       ;; (gathering x ... (for ...) ...)
       `(,op ,@args ,(make-form more)))
      ((op args . more)
       (:when (symbolp op))
       ;; (gathering x ... for ... ...)
       `(,op ,args ,(make-form more)))
      (otherwise
       (error "bad clause tail ~S from ~S" ct clauses)))))
||#

#||
(gather (* x x)
  for (x (in '(1 2 t 3)))
  when (numberp x)
  when (oddp x))

(gather (* x x)
  (for (x (in '(1 2 t 3))))
  (when (numberp x))
  (when (oddp x)))

(gather x
  for (l (in '((1 nil 2) (3 4 t))))
  for (x l)
  when (numberp x))
||#
