;;;; Python-style (not really) decorators
;;;

(org.tfeb.tools.require-module:needs
 ((:org.tfeb.hax.utilities
   :org.tfeb.dsm
   :org.tfeb.hax.spam)
  :compile t))

(defpackage :org.tfeb.toys.decorators
  (:use :cl :org.tfeb.hax.utilities :org.tfeb.dsm :org.tfeb.hax.spam)
  (:export
   #:decorator-error
   #:define-decorator-dispatcher
   #:make-decorator-readtable))

(in-package :org.tfeb.toys.decorators)

(provide :org.tfeb.toys.decorators)

(define-condition decorator-error (reader-error simple-error)
  ())

(defun decorator-error (format-control &rest format-arguments)
  (error 'decorator-error
         :format-control format-control
         :format-arguments format-arguments))

(defmacro decorating (form &body decorators &key (pre '()) (post '()))
  ;; Wrap a form in pre & post decorators.  Note this doesn't care
  ;; about values at all: because PROGN is safe at top-level, but
  ;; other things are not in general.  So things which provide post
  ;; decorators will need to ensure they return the right value
  (declare (ignore decorators))
  (if (and (null pre) (null post))
      form
    `(progn ,@pre ,form ,@post)))

;;; Dispatchers are either defined like this, or may be just
;;; functions.  The reason for this is so we can name dispatchers
;;; things like INLINE.
;;;

(defvar *decorator-dispatch-table* (make-hash-table))

(defmacro define-decorator-dispatcher (name (form &rest ll-tail)
                                    &body decls/forms)
  ;; Define a dispatch function.  Dispatch functions take the form,
  ;; any existing pre & post decorators, and any prefix argument, and
  ;; should return a new form and two new lists of decorators.
  (multiple-value-bind (decls forms) (parse-simple-body decls/forms)
    `(progn
       (setf (gethash ',name *decorator-dispatch-table*)
             (lambda (,form ,@ll-tail)
               ,@decls
               (block name
                 ,@forms)))
       ',name)))

(defun decorator-dispatcher (stream char prefix)
  (declare (ignore char))
  ;; I am very unsure about the recursiveness of these reads: I think
  ;; they should be, but I am not sure
  (if *read-suppress*
      (progn (read stream t nil t) (read stream t nil t) nil)
    (let ((dispatcher-spec (read stream t nil t))
          (form (read stream t nil t)))
      (multiple-value-bind (dispatcher options)
          (destructuring-match dispatcher-spec
            ((d . opts)
             (:when (symbolp d))
             (values (gethash d *decorator-dispatch-table*
                              (and (fboundp d) (symbol-function d)))
                     opts))
            (d
             (:when (symbolp d))
             (values (gethash d *decorator-dispatch-table*
                              (and (fboundp d) (symbol-function d)))
                     '()))
            (otherwise
             (decorator-error "bad dispatcher spec ~S" dispatcher-spec)))
        (unless dispatcher
          (decorator-error "no dispatcher for ~S" dispatcher-spec))
        (multiple-value-bind (it pre post)
            (destructuring-match form
              ((decorating it &key pre post)
               (:when (eq decorating 'decorating))
               (values it pre post))
              (it
               (values it '() '())))
          (let ((returned (multiple-value-list
                           (funcall dispatcher it
                                    :options options
                                    :pre pre
                                    :post post
                                    :prefix prefix))))
            (destructuring-match returned
              ((new-it new-pre new-post)
               (:when (and (matchp new-pre (list-of (any)))
                           (matchp new-post (list-of (any)))))
               `(decorating ,new-it :pre ,new-pre :post ,new-post))
              ((new-it new-pre)
               (:when (and (matchp new-pre (list-of (any)))))
               `(decorating ,new-it :pre ,new-pre :post post))
              ((new-it)
               `(decorating ,new-it :pre pre :post post))
              (otherwise
               (decorator-error "unexpected returns from decorator function for ~S"
                                dispatcher-spec)))))))))

(defun make-decorator-readtable (&key
                                 (from *readtable*)
                                 (to nil)
                                 ;; this will clash with READ-PACKAGE
                                 (decorator-char #\@)
                                 (clobber nil))
  (let ((rt (copy-readtable from to)))
    (when (and (not clobber)
               (get-dispatch-macro-character #\# decorator-char rt))
      (error "Someone is already using #~A" decorator-char))
    (set-dispatch-macro-character
     #\# decorator-char #'decorator-dispatcher rt)
    rt))

;;; Some decorators
;;;

(define-decorator-dispatcher inline (form &key pre &allow-other-keys)
  (declare (ignore prefix))
  (destructuring-match form
    ((_ name . _)
     (values form (cons `(declaim (inline ,name)) pre)))
    (otherwise
     (decorator-error "~S doesn't look like a definition" form))))

(define-decorator-dispatcher notinline (form &key pre &allow-other-keys)
  (declare (ignore prefix))
  (destructuring-match form
    ((_ name . _)
     (values form (cons `(declaim (notinline ,name)) pre)))
    (otherwise
     (decorator-error "~S doesn't look like a definition" form))))

#||
(define-decorator-dispatcher traced (fdef &key options &allow-other-keys)
  (destructuring-match options
    ((&key (to '*trace-output*)
           (when 't))
     (destructuring-match fdef
       ((defun name arglist &body doc/decls/forms)
        (:when (eq defun 'defun))
        (multiple-value-bind (doc decls forms) (parse-docstring-body doc/decls/forms)
          (let ((<when> (make-symbol "WHEN"))
                (<s> (make-symbol "S")))
            `(defun ,name ,arglist
               ,@(if doc (list doc))
               ,@decls
               (let ((,<when> ,when)
                     (,<s> ,to))
                 (when ,<when>
                   (format ,<s> "[~S" ',name))
                 (multiple-value-prog1
                     (progn
                       ,@forms)
                   (when ,<when>
                     (format ,<s> "]~%" ',name))))))))
       (otherwise
        (decorator-error "not a function definition: ~S" fdef))))
    (otherwise
     (decorator-error "unexpected options for traced ~S" options))))
||#