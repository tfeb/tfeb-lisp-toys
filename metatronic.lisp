;;;; Metatronic macros
;;;

(defpackage :org.tfeb.toys.metatronic
  (:use :cl)
  (:export
   #:define-metatronic-macro
   #:metatronize))

(in-package :org.tfeb.toys.metatronic)

(provide :org.tfeb.toys.metatronic)

(defun metatronize (form &optional (metatab '()))
  ;; This has hard-wired knowledge of what a metatronic variable looks
  ;; like.
  "Return a metatronic version of FORM, and the table of variables

Arguments are FORM and an optional table.

This only looks at list structure."
  (labels ((rewrite (this)
             (typecase this
               (symbol
                (let* ((n (symbol-name this))
                       (l (length n)))
                  (cond
                   ((< l 2)
                    this)
                   ((and (char= (char n 0) #\<)
                         (char= (char n (1- l)) #\>))
                    (if (= l 2)
                        (make-symbol "<>")
                      (let ((found (assoc this metatab)))
                        (if found
                            (cdr found)
                          (let ((new (make-symbol n)))
                            (setf metatab (acons this new metatab))
                            new)))))
                   (t
                    this))))
               (cons
                (cons (rewrite (car this))
                      (rewrite (cdr this))))
               ;; Not going to handle arrays etc because it is a lot
               ;; of work for very tiny / 0 benefit.
               (t
                this))))
    (values (rewrite form) metatab)))

(defmacro define-metatronic-macro (name (&rest args) &body doc/forms)
  "Define a metatronic macro

This is exactly like DEFMACRO but metatronic symbols are gensymized,
when they occur directly in list structure.

Note that metatronic symbols are *not* gensymized in arrays,
structures or what have you as it's just too hard.  Use
LOAD-TIME-VALUE to construct a literal at load time if you really need
this."
  (multiple-value-bind (metatronic-args table) (metatronize args)
    (let ((metatronic-doc/forms (metatronize doc/forms table)))
      `(defmacro ,name ,metatronic-args ,@metatronic-doc/forms))))

#||
(define-metatronic-macro do-file ((lv file) &body forms)
  `(let ((<file> ,file))
     (with-open-file (<in> <file>)
       (doing ((,lv (read-line <in> nil <in>)))
              ((eq ,lv <in>) <file>)
         ,@forms))))
||#
