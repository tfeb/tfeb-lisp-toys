;;;; Rudimentary tests for metatronic macros
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.toys.metatronic
  :compile t)
  #+Quicklisp
  ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.toys.metatronic/test
  (:use :cl :org.tfeb.toys.metatronic :org.shirakumo.parachute))

(in-package :org.tfeb.toys.metatronic/test)

(define-test "org.tfeb.toys.metatronic")

(define-test ("org.tfeb.toys.metatronic" "simple")
  (macrolet/m ((eq<> () '(eq '<> '<>))
               (eq<x> () '(eq '<x> '<x>)))
    (false (eq<>))
    (true (eq<x>)))
  (macrolet/m ((with-anon (bindings &body forms)
                 `(let ((<x> 1))
                    (let ,bindings
                      ,@forms
                      <x>))))
    (is = 1 (with-anon ((<x> 2))
              (declare (ignore <x>)))))
  (macrolet/m ((with-value ((v) &body forms)
                 `(let ((<x> ,v))
                    (values (progn ,@forms) <x>))))
    (is-values (let ((<x> 1))
                 (with-value (2)
                   <x>))
      (= 1)
      (= 2))))

(test "org.tfeb.toys.metatronic" :report 'summary)
