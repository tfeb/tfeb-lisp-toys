;;;; Spaghetti tests
;;;


#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.toys.spaghetti :compile t)
  #+Quicklisp
  ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.toys.spaghetti/test
  (:use :cl :org.tfeb.toys.spaghetti :org.shirakumo.parachute))

(in-package :org.tfeb.toys.spaghetti/test)

(define-test "org.tfeb.toys.spaghetti")

(define-test ("org.tfeb.toys.spaghetti" "label forms")
  (finish
   (labelling ()
     (end)
     (error "foo")
     (label end)))
  (finish
   (labelling ((a 1))
     (end 2)
     (error "foo")
     (label end (a))
     a))
  (is eql (labelling ((a 1))
            (end 2)
            (error "foo")
            (label end (a))
            a)
      2))

(define-test ("org.tfeb.toys.spaghetti" "labelling / labelling*")
  (is-values (let ((a 1))
               (labelling ((a 2) (b a))
                 (values a b)))
    (eql 2)
    (eql 1))
  (is-values (let ((a 1))
               (declare (ignorable a))
               (labelling* ((a 2) (b a))
                 (values a b)))
    (eql 2)
    (eql 2)))

(define-test ("org.tfeb.toys.spaghetti" "label / label*")
  (is-values (labelling ((a 1) (b 2))
               (end)
               (label end (&aux (a (1+ a)) (b (1+ a))))
               (values a b))
    (eql 2)
    (eql 2))
  (is-values (labelling ((a 1) (b 2))
               (end)
               (label* end (&aux (a (1+ a)) (b (1+ a))))
               (values a b))
    (eql 2)
    (eql 3)))

(define-test ("org.tfeb.toys.spaghetti" "jumps")
  (finish
   (labelling ()
     (end)
     (error "oops")
     (label end ()))))

(define-test ("org.tfeb.toys.spaghetti" "spaghetti")
  (is equal (labelling ((a 1) (b '()))
              (label start (a))
              (push a b)
              (when (< a 3)
                (start (1+ a)))
              (incf a)
              (if (< a 10)
                  (start 10)
                (progn (push 13 b)
                  (end 13)))
              (error "not reached")
              (label end (a))
              b)
      '(13 10 3 2 1)))

(define-test ("org.tfeb.toys.spaghetti" "arglists")
  ;; Is this the right way to do this?
  (macrolet ((le (&body label-forms)
               `(macroexpand '(labelling ((x 1) (y 2) (z 3))
                                ,@label-forms))))
    (fail (le (label foo (&key (x 2) &optional (y 0)))))
    (fail (le (label foo (&aux (x 2) &key y))))
    (fail (le (label foo (&aux (x 2) &optional y))))
    (finish (le (label foo (x &optional (y 1) &key (z 2)))))
    (finish (le (label foo (x &optional (y 1) &aux (z 2)))))
    (finish (le (label foo (&key (x 1) &aux (y 2)))))))

(test "org.tfeb.toys.spaghetti" :report 'summary)
