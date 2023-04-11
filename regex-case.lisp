;;;; Regex case construct, experiment
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 #+Quicklisp
 ("cl-ppcre" :fallback ql:quickload)
 #-Quicklisp
 "cl-ppcre"
 ((:org.tfeb.hax.metatronic
   :org.tfeb.hax.collecting)
  :compile t))

(defpackage :org.tfeb.toys.regex-case
  (:use
   :cl
   :cl-ppcre
   :org.tfeb.hax.metatronic :org.tfeb.hax.collecting)
  (:export
   #:regex-case-error
   #:regex-case))

(in-package :org.tfeb.toys.regex-case)

(provide :org.tfeb.toys.regex-case)

(define-condition regex-case-error (program-error simple-error)
  ())

(defun regex-case-error (format &rest arguments)
  (error 'regex-case-error
         :format-control format
         :format-arguments arguments))

(defmacro/m regex-case (string &body clauses)
  "A CASE-like construct for regular expressions"
  `(let ((<string> ,string))
     (block <regex-case>
       ,@(maplist
          (lambda (ctail)
            (destructuring-bind (pattern (&rest binding-specs
                                                &key (match nil)
                                                (match-start nil)
                                                (match-end nil)
                                                (registers nil)
                                                (register-starts nil)
                                                (register-ends nil))
                                         &body decls/forms)
                (first ctail)
              (unless (and (symbolp match)
                           (symbolp match-start)
                           (symbolp match-end)
                           (every #'symbolp registers)
                           (symbolp register-starts)
                           (symbolp register-ends))
                (regex-case-error "regex binding variables aren't"))
              (case pattern
                ((otherwise t)
                 (when (not (null binding-specs))
                   (regex-case-error "register bindings for fallback clause"))
                 (unless (null (rest ctail))
                   (regex-case-error "fallback clause not last"))
                 `(return-from <regex-case>
                    (locally ,@decls/forms)))
                (otherwise
                 `(multiple-value-bind (<ms> <me> <rsv> <rev>)
                      (scan (load-time-value (create-scanner ',pattern))
                            <string>)
                    (declare (ignorable <ms> <me> <rsv> <rev>))
                    (when <ms>
                      (unless (>= (length <rsv>) ,(length registers))
                        (regex-case-error "more register variables than registers"))
                      (return-from <regex-case>
                        (let (,@(if match-start `((,match-start <ms>)) '())
                              ,@(if match-end `((,match-end <me>)) '())
                              ,@(if register-starts `((,register-starts <rsv>)) '())
                              ,@(if register-ends `((,register-ends <rev>)) '())
                              ,@(if match `((,match (subseq <string> <ms> <me>)))
                                  '())
                              ,@(collecting
                                  (do ((rt registers (rest rt))
                                       (i 0 (1+ i)))
                                      ((null rt))
                                    (let ((rv (first rt)))
                                      (when (and rv (not (string= (symbol-name rv) "_")))
                                        (collect `(,rv
                                                   (subseq <string>
                                                           (aref <rsv> ,i)
                                                           (aref <rev> ,i)))))))))
                          ,@decls/forms))))))))
          clauses))))
