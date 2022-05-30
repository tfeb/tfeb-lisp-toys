;;;; Tests for slog
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.toys.slog/test
  (:use :cl
   :org.shirakumo.parachute))

(in-package :org.tfeb.toys.slog/test)

(define-test "org.tfeb.toys.slog")

(dolist (test '("test-slog-blackbox" "test-slog-whitebox"))
  (load (if *load-pathname*
            (merge-pathnames test *load-pathname*)
          test)))

(test "org.tfeb.toys.slog" :report 'summary)
