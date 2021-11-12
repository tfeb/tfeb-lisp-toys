;;;; Spaghetti code
;;; or: GOTO passing arguments
;;; or: tentacles
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.collecting
 :org.tfeb.hax.iterate)

(defpackage :org.tfeb.toys.spaghetti
  (:use :cl :org.tfeb.hax.collecting :org.tfeb.hax.iterate)
  (:export
   #:labelling
   #:labelling*
   #:label
   #:label*))

(in-package :org.tfeb.toys.spaghetti)

(provide :org.tfeb.toys.spaghetti)

(defun labelify-body (forms)
  ;; Return the canonical body, the label and label* forms
  (with-collectors (body label-form label*-form)
    (do* ((tail forms (rest tail))
          (form (first tail) (first tail)))
         ((null tail))
      (cond
       ((and (consp form) (member (first form) '(label label*)))
        (case (length form)
          (2
           (unless (symbolp (second form))
             (error "mutant label ~S" form))
           (body (second form))
           (case (first form)
             (label (label-form (list (second form) '())))
             (label* (label*-form (list (second form) '())))))
          (3
           (unless (and (symbolp (second form))
                        (listp (third form)))
             (error "mutant label ~S" form))
           (body (second form))
           (case (first form)
             (label (label-form (rest form)))
             (label* (label*-form (rest form)))))
          (otherwise
           (error "mutant label ~S" form))))
       ((null (rest tail))
        (body `(return ,form)))
       ((symbolp form)
        (body `(progn ,form)))
       (t
        (body form))))))

(defun anonymize-label-arglist (label-arglist)
  ;; Validate a label arglist and return the anonymized version, a
  ;; list of variables and a list of mapped variables.  This also
  ;; deals with quoting the default values.
  (let ((seen '()))
    (with-collectors (arglist variable mapped)
      (labels
          ((make-anonymous-variable (var)
             (if (member var seen)
                 (error "duplicate variable ~S" var)
               (progn
                 (push var seen)
                 (make-symbol (symbol-name var)))))
           ;; This is repetitive and horrible
           (parse-normals (tail)
             (when (not (null tail))
               (destructuring-bind (varspec . more) tail
                 (case varspec
                   (&optional
                    (arglist '&optional)
                    (parse-optionals more))
                   (&key
                    (arglist '&key)
                    (parse-keywords more))
                    (&aux
                     (arglist '&aux)
                     (parse-auxes more))
                    (otherwise
                     (typecase varspec
                       (symbol
                        (when (member varspec lambda-list-keywords)
                          (error "bad lambda list keyword ~S in ~S"
                                 varspec label-arglist))
                        (let ((v (make-anonymous-variable varspec)))
                          (arglist v)
                          (variable varspec)
                          (mapped v))
                        (parse-normals more))
                       (t
                        (error "mutant ~S in ~S" varspec label-arglist))))))))
           (parse-optionals (tail)
             (when (not (null tail))
               (destructuring-bind (varspec . more) tail
                 (case varspec
                   (&key
                    (arglist '&key)
                    (parse-keywords more))
                   (&aux
                    (arglist '&aux)
                    (parse-auxes more))
                   (otherwise
                    (typecase varspec
                      (symbol
                        (when (member varspec lambda-list-keywords)
                          (error "bad lambda list keyword ~S in ~S"
                                 varspec label-arglist))
                        (let ((v (make-anonymous-variable varspec)))
                          (arglist v)
                          (variable varspec)
                          (mapped v))
                        (parse-optionals more))
                      (list
                       (unless (and (<= 1 (length varspec) 2)
                                    (symbolp (first varspec))
                                    (not (member (first varspec)
                                                 lambda-list-keywords)))
                         (error "bad optional specification ~S in ~S"
                                varspec label-arglist))
                       (let* ((var (first varspec)) ;not sure abut destructuring-bind
                              (init (second varspec))
                              (v (make-anonymous-variable var)))
                         (arglist (list v `',init))
                         (variable var)
                         (mapped v))
                       (parse-optionals more))
                      (t
                       (error "mutant optional argument spec ~S in ~S"
                              varspec label-arglist))))))))
           (parse-keywords (tail)
             (when (not (null tail))
               (destructuring-bind (varspec . more) tail
                 (case varspec
                   (&aux
                    (arglist '&aux)
                    (parse-auxes more))
                   (otherwise
                    (typecase varspec
                      (symbol
                        (when (member varspec lambda-list-keywords)
                          (error "bad lambda list keyword ~S in ~S"
                                 varspec label-arglist))
                        (let ((v (make-anonymous-variable varspec)))
                          (arglist v)
                          (variable varspec)
                          (mapped v))
                        (parse-keywords more))
                      (list
                       (unless (and (<= 1 (length varspec) 2)
                                    (symbolp (first varspec))
                                    (not (member (first varspec)
                                                 lambda-list-keywords)))
                         (error "bad keyword argument specification ~S in ~S"
                                varspec label-arglist))
                       (let* ((var (first varspec)) ;not sure abut destructuring-bind
                              (init (second varspec))
                              (v (make-anonymous-variable var)))
                         (arglist (list v `',init))
                         (variable var)
                         (mapped v))
                       (parse-keywords more))
                      (t
                       (error "mutant keyword argument spec ~S in ~S"
                              varspec label-arglist))))))))
           (parse-auxes (tail)
             (when (not (null tail))
               (destructuring-bind (varspec . more) tail
                 (typecase varspec
                   (symbol
                    (when (member varspec lambda-list-keywords)
                      (error "bad lambda list keyword ~S in ~S"
                             varspec label-arglist))
                    (let ((v (make-anonymous-variable varspec)))
                      (arglist v)
                      (variable varspec)
                      (mapped v))
                        (parse-auxes more))
                   (list
                    (unless (and (<= 1 (length varspec) 2)
                                 (symbolp (first varspec))
                                 (not (member (first varspec)
                                              lambda-list-keywords)))
                      (error "bad auxiliary argument specification ~S in ~S"
                             varspec label-arglist))
                    (let* ((var (first varspec)) ;not sure abut destructuring-bind
                           (init (second varspec))
                           (v (make-anonymous-variable var)))
                      (arglist (list v `',init))
                      (variable var)
                      (mapped v))
                    (parse-auxes more))
                   (t
                    (error "mutant auxiliary argument spec ~S in ~S"
                           varspec label-arglist)))))))
        (parse-normals label-arglist)))))

(defun label-form->macrolet-form (label-form &optional (setq 'psetq))
  (destructuring-bind (label-name label-arglist) label-form
    (multiple-value-bind (arglist variables mapped)
        (anonymize-label-arglist label-arglist)
      `(,label-name
        ,arglist
        `(progn
           (,',setq
            ,@(mapcan #'list ',variables (list ,@mapped)))
           (go ,',label-name))))))

(defmacro labelling (bindings &body decls/forms)
  ;; like LET
  (multiple-value-bind (body label-forms label*-forms) (labelify-body decls/forms)
    `(macrolet ,(append (mapcar #'label-form->macrolet-form label-forms)
                        (mapcar (lambda (form)
                                  (label-form->macrolet-form form 'setq))
                          label*-forms))
       (prog ,bindings
         ,@body))))

(defmacro labelling* (bindings &body decls/forms)
  ;; like LET*
  (multiple-value-bind (body label-forms label*-forms) (labelify-body decls/forms)
    `(macrolet ,(append (mapcar #'label-form->macrolet-form label-forms)
                        (mapcar (lambda (form)
                                  (label-form->macrolet-form form 'setq))
                          label*-forms))
       (prog* ,bindings
         ,@body))))
