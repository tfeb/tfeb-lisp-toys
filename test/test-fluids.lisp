;;;; Fluid tests
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.toys.fluids :compile t)
  #+Quicklisp
  ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.toys.fluids/test
  (:use :cl :org.tfeb.toys.fluids :org.shirakumo.parachute))

(in-package :org.tfeb.toys.fluids/test)

(define-test "org.tfeb.toys.fluids")

(define-test ("org.tfeb.toys.fluids" "simple")
  (let ((f (make-fluid 'foo))
        (g (make-fluid)))
    ;; check names and binding status
    (is eql (fluid-name f) 'foo)
    (is eql (fluid-name g) nil)
    (is-values (fluid-boundp f)
      (eql nil)
      (eql nil))
    (is-values (fluid-boundp f)
      (eql nil)
      (eql nil))))

(define-test ("org.tfeb.toys.fluids" "local and global")
  (let ((f (make-fluid 'foo 1)))
    (is eql (fluid-value f) 1)
    (fluid-let ((f 2))
      (is eql (fluid-value f) 2)
      (is eql (fluid-value f t) 1)
      (finish (setf (fluid-value f) 9))
      (is eql (fluid-value f) 9)
      (is eql (fluid-value f t) 1)
      (finish (setf (fluid-value f t) 8))
      (is eql (fluid-value f) 9)
      (is eql (fluid-value f t) 8))
    (is eql (fluid-value f) 8)))

(define-test ("org.tfeb.toys.fluids" "boundp")
  (let ((f (make-fluid 'foo 1))
        (g (make-fluid)))
    (is-values (fluid-boundp f)
      (eql t) (eql t))
    (is-values (fluid-boundp g)
      (eql nil) (eql nil))
    (fluid-let ((f 1) (g 2))
      (is-values (fluid-boundp g)
        (eql t) (eql nil))
      (is-values (fluid-boundp f)
        (eql t) (eql t)))
    (is-values (fluid-boundp f t)
      (eql t) (eql nil))
    (is-values (fluid-boundp g t)
      (eql nil) (eql nil))
    (fluid-let ((f 1) (g 2))
      (is-values (fluid-boundp f t)
        (eql t) (eql t))
      (is-values (fluid-boundp g t)
        (eql nil) (eql t)))))

(define-test ("org.tfeb.toys.fluids" "makunbound")
  (let ((f (make-fluid)))
    (is eql (fluid-makunbound f) f)
    (fluid-let ((f 1))
      (fail (fluid-makunbound f) bound-fluid-error))
    (fail (setf (fluid-value f) 2) unbound-fluid-error)
    (finish (setf (fluid-value f t) 2))
    (is-values (fluid-boundp f t)
      (eql t) (eql nil))
    (finish (fluid-makunbound f))
    (is-values (fluid-boundp f t)
      (eql nil) (eql nil))))

(test "org.tfeb.toys.fluids")
