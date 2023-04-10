;;;; Stupid C-style enums (Zyni)
;;;
;;; Only reason for enums like this is to be compatible with C since Lisp has symbols.
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ((:org.tfeb.dsm :org.tfeb.hax.simple-loops)
  :compile t))

(defpackage :org.tfeb.toys.enumerations
  (:use :cl :org.tfeb.dsm :org.tfeb.hax.simple-loops)
  (:export
   #:define-enumeration
   #:enumeration-case))

(in-package :org.tfeb.toys.enumerations)

(provide :org.tfeb.toys.enumerations)

;;; Enumeration information is a property list with keys:
;;;  :map is an alist from (name . value);
;;;  :constants is a list of constant names in map order;
;;;  :base is the base enumeration or NIL.
;;;

(defun enumeration-name-p (n)
  (if (get n 'enumeration-information) t nil))

(defun enumeration-information (n)
  (get n 'enumeration-information nil))

(defun (setf enumeration-information) (information n)
  (setf (get n 'enumeration-information) information))

(defun enumeration-constant-of (n)
  (get n 'enumeration-constant-of))

(defun (setf enumeration-constant-of) (of n)
  (setf (get n 'enumeration-constant-of) of))

(defmacro define-enumeration (name/options &body clauses)
  "Define a C-style enumeration of integers

NAME/OPTIONS is either the name of the enumeration, or (name . options)
where the options are

- :SEPARATOR sep to specify the separator for constant names, the default being #\.
- :PACKAGE p to specify the package into which constant names are interned, the defaily
  being *PACKAGE*
- :BASE b to specify a base enumerator from which this one is derived.

A type for the name is defined.

See the documentation for a bit more information."
  (multiple-value-bind (name separator package base)
      (destructuring-match name/options
        ((name &key (separator #\.) (package *package*) (base nil))
         (values name separator package base))
        (name
         (values name #\. *package* nil)))
    (assert (and name (symbolp name)) (name) "enumeration name must be a non-NIL symbol")
    (when base
      (unless (enumeration-name-p base)
        (error "base ~S isn't" base)))
    (let* ((map
            (let ((parent-map (if base (getf (enumeration-information base) ':map))))
              (looping ((value (1+ (reduce (lambda (&optional (v1 -1) (v2 -1))
                                             (max v1 v2))
                                           parent-map
                                           :key #'cdr)))
                        (ctail clauses)
                        (map '()))
                (if (null ctail)
                    (return (append parent-map (nreverse map)))
                  (destructuring-match (first ctail)
                    ((name v)
                     (values (1+ v) (rest ctail) (cons (cons name v) map)))
                    (name
                     (values (1+ value) (rest ctail) (cons (cons name value) map))))))))
           (cns (mapcar (lambda (e)
                          (intern (format nil "~A~A~A"
                                          (string name)
                                          (string separator)
                                          (string (car e)))
                                  package))
                        map)))
      (setf (enumeration-information name)
            `(:map ,map
              :constants ,cns
              :base ,base))
      `(eval-when (:load-toplevel :compile-toplevel :execute)
         ,@(mapcan (lambda (e cn)
                     `((defconstant ,cn ,(cdr e))
                       (setf (enumeration-constant-of ',cn) ',name)))
                   map cns)
         (deftype ,name ()
           '(member ,@(mapcar #'cdr map)))))))

(defmacro enumeration-case (thing/of &body clauses)
  "A CASE construct for enumerations.

Each clause shiould be (key/s ...) where key/s is either an
enumeration constant, a list of them, an integer or one of OTHERWISE
or T for the default case.

If THING/OF is (thing :OF enumeration) then you can use the short form
of the names for the enumeration constants for enumeration as well.

There is no type check, even in the latter case."
  (multiple-value-bind (thing of)
      (destructuring-match thing/of
        ((thing &key of)
         (when of
           (unless (enumeration-name-p of)
             (error "~S does not name an enumeration" of))
           (values thing of)))
        (thing (values thing nil)))
    (flet ((valof (key)
             (etypecase key
               (integer key)
               (symbol
                (cond
                 (of
                  (let ((found (assoc key (getf (enumeration-information of) ':map))))
                    (cond
                     (found (cdr found))
                     ((eq (enumeration-constant-of key) of)
                      (symbol-value key))
                     (t (error "~S is not a key for ~S" key of)))))
                 ((boundp key)
                  (symbol-value key))
                 (t (error "key ~S is unbound" key)))))))
      `(case ,thing
         ,@(mapcar (lambda (clause)
                     (destructuring-bind (key/s &body forms) clause
                       `(,(cond
                           ((member key/s '(otherwise t))
                            key/s)
                           ((listp key/s)
                            (mapcar #'valof key/s))
                           ((symbolp key/s)
                            (valof key/s))
                           (t
                            (error "what even is ~S" key/s)))
                         ,@forms)))
                   clauses)))))

#||
(define-enumeration monster
  big
  small
  tentacled
  invincable)

(defun action (monster)
  (enumeration-case (monster :of monster)
    ((big small) 'fight)
    ((tentacled invincable) 'run)
    (otherwise 'unknown)))

(define-enumeration (nasty-monster :base monster)
  nasty
  very-nasty)

(defun nasty-action (n)
  (enumeration-case (n :of nasty-monster)
    (nasty 'run-fast)
    (nasty-monster.very-nasty 'die)
    (otherwise
     'fight)))
||#
