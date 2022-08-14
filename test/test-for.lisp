;;;; Tests for FOR / GATHER
;;;
;;; These are incomplete and don't test most of the interesting cases
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ((:org.tfeb.toys.for
   :org.tfeb.hax.collecting)
  :compile t)
  #+Quicklisp
  ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.toys.for/test
  (:use :cl :org.tfeb.toys.for :org.tfeb.hax.collecting :org.shirakumo.parachute))

(in-package :org.tfeb.toys.for/test)

(define-test "org.tfeb.toys.for")

(define-test ("org.tfeb.toys.for" "sanity")
  (is equal '(0 1 2 3)
      (collecting
        (for (x '(0 1 2 3))
          (collect x))))
  (is equal '(0 1 2 3)
      (collecting
        (for ((x '(0 1 2 3)))
          (collect x))))
  (is equal '(0 a 1 b 2 c 3 d)
      (collecting
        (for ((x '(0 1 2 3))
              (y '(a b c d)))
          (collect x)
          (collect y))))
  (is equal '(0 1 2 3)
      (gather x
        for (x '(0 1 2 3))))
  (is equal '(0 2)
      (gather x
        for (x '(0 1 2 3))
        when (evenp x)))
  (is equal '(0 1 2 3)
      (gather i
        for (i 4)))
  (is equal '(0 1 2 3)
      (gather i
        for ((i 4) (j 5))))
  (is equal '((0 a) (1 b) (2 c) (3 d))
      (gather (list i x)
        for ((i 4)
             (x '(a b c d e))))))

(define-test ("org.tfeb.toys.for" "simple-lists")
  (is equal '(0 1)
      (gather x
        for (x (iterator '(0 1)))))
  (is equal '(1)
      (gather x
        for (x (iterator '(0 1) :start 1))))
  (is equal '(0 2 4)
      (gather x
        for (x (iterator '(0 1 2 3 4 5) :step 2))))
  (is equal '(1 3 5)
      (gather x
        for (x (iterator '(0 1 2 3 4 5) :step 2 :start 1))))
  (is equal '(0 1)
      (gather x
        for (x (iterator '(0 1 2 3 4) :end 2))))
  (is equal '()
      (gather x
        for (x (iterator '(0 1 2) :end 0)))))

(define-test ("org.tfeb.toys.for" "cyclic-lists")
  (is equal '(0 1 0 1)
      (gather x
        for ((x (iterator '(0 1) :cyclic t))
             (i 4))))
  (is equal '(0 0 0 0)
      (gather x
        for ((x (iterator '(0 1) :step 2 :cyclic t))
             (i 4))))
    (is equal '(0 1 0 1)
      (gather x
        for ((x (iterator '(0 1) :step 3 :cyclic t))
             (i 4))))
    (is equal '(0 0 0 0)
        (gather x
          for ((x (iterator '(0 1) :step 3 :cyclic t :end 1))
               (i 4))))
    (is equal '(2 3 2 3)
        (gather x
          for ((x (iterator '(1 2 3 4) :start 1 :end 3 :cyclic t))
               (i 4)))))

(define-test ("org.tfeb.toys.for" "simple-vectors")
  (let ((v #(0 1 2)))
    (is equal '(0 1 2)
        (gather i for (i v)))
    (is equal '(1 2)
        (gather i for (i (iterator v :start 1))))
    (is equal '(0 1)
        (gather i for (i (iterator v :end 2))))))

(define-test ("org.tfeb.toys.for" "simple-reals")
  (is equal '(0 1 2 3)
      (gather i for (i 4)))
  (is equal '()
      (gather i for (i -4)))
  (is equal (collecting
              (do ((i 0 (1+ i))
                   (x 0.0f0 (+ x 1)))
                  ((= i 4))
                (collect x)))
      (gather x for (x 4.0f0)))
  (is equal '(4 3 2 1)
      (gather i
        for (i (iterator 0 :start 4 :step -1))))
  (is equal '(0 1 0 1)
      (gather i
        for ((i (iterator 2 :cyclic t))
             (j 4))))
  (is equal '(0 0 0 0)
      (gather i
        for ((i (iterator 2 :cyclic t :step 2))
             (j 4))))
  (is equal '(0 1 0 1)
      (gather i
        for ((i (iterator 2 :cyclic t :step 3))
             (j 4))))
  (is equal '(0 -2)
      (gather i
        (for (i (iterator -4 :step -2))))))

(define-test ("org.tfeb.toys.for" "appending")
  (is equal '(0 1 2 3)
      (gather i
        for (i (append-iterators (iterator '(0 1))
                                 (iterator '(2 3)))))))

(test "org.tfeb.toys.for" :report 'summary)
