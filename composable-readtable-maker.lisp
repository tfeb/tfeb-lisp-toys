;;;; Composable readtable makers
;;;

;;; This needs conduit packages
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.conduit-packages
 :org.tfeb.hax.iterate
 :org.tfeb.hax.collecting)

(defpackage :org.tfeb.toys.composable-readtable-maker/implementation
  ;; The implementation is too complicated and not (yet) documented
  (:use :org.tfeb.clc :org.tfeb.hax.iterate :org.tfeb.hax.collecting)
  (:export
   #:crm-user-error
   #:crm-internal-error
   #:composable-readtable-maker
   #:dispatching-macro-characters
   #:dispatching-macro-character-specifications
   #:macro-character-specifications
   #:crm-readtable-case
   #:character-syntax-map
   #:special-orders
   #:add-dispatching-macro-character-to-make
   #:add-dispatching-macro-characters-to-maker
   #:add-dispatching-macro-character-specification-to-maker
   #:add-dispatching-macro-character-specifications-to-maker
   #:add-macro-character-specification-to-maker
   #:add-macro-character-specifications-to-maker
   #:add-character-syntax-map-entry-to-maker
   #:add-character-syntax-map-entries-to-maker
   #:add-special-order-to-maker
   #:add-special-orders-to-maker
   #:make-composable-readtable-maker
   #:compose-composable-readtable-makers
   #:make-readtable-from-composable-readtable-maker))

(in-package :org.tfeb.toys.composable-readtable-maker/implementation)

(defpackage :org.tfeb.toys.composable-readtable-maker
  ;; This is the public interface which is documented, and is less
  ;; insane.
  (:use)
  (:extends/including
   :org.tfeb.toys.composable-readtable-maker/implementation
   #:crm-user-error
   #:crm-internal-error
   #:make-composable-readtable-maker
   #:compose-composable-readtable-makers
   #:make-readtable-from-composable-readtable-maker))

(#+org.tfeb.tools.require-module org.tfeb.tools.require-module:provide-module
 #-org.tfeb.tools.require-module provide
 :org.tfeb.toys.composable-readtable-maker)

(deftype finite-list-of (&rest types)
  ;; a list of a known number of types
  (if (null types)
      'null
    `(cons ,(first types) (finite-list-of ,@(rest types)))))

(defun remove-properties (properties plist)
  ;; remove properties from a plist
  (do* ((ptail plist (cddr ptail))
        (accum (if (and (not (null ptail))
                        (not (member (first ptail) properties)))
                   (list (second ptail) (first ptail))
                 '())
               (if (and (not (null ptail))
                        (not (member (first ptail) properties)))
                   (list* (second ptail) (first ptail) accum)
                 accum)))
       ((null ptail) (nreverse accum))))

(define-condition crm-error (simple-error)
  ())

(define-condition crm-internal-error (crm-error)
  ()
  (:documentation
   "Conditon type for internal errors in composable-readtable-maker"))

(define-condition crm-user-error (crm-error)
  ()
  (:documentation
   "Condition type for user errors signaled by composable-readtable-maker"))

(defun crm-user-error (fmt &rest args)
  (error 'crm-user-error
         :format-control fmt
         :format-arguments args))

(defun crm-internal-error (fmt &rest args)
  (error 'crm-internal-error
         :format-control fmt
         :format-arguments args))

;;; Syntax tree accessors & walker for them
;;; (There is no strong reason for these to be inlined, but why not?)
;;;
(declaim (inline make-syntax-node make-syntax-root-node
                 syntax-node-key syntax-node-children
                 (setf syntax-node-children)
                 syntax-root-node-p))
(defun make-syntax-node (key &rest children)
  (cons key children))
(defun syntax-node-key (node)
  (car node))
(defun make-syntax-root-node (&rest children)
  (cons nil children))
(defun syntax-root-node-p (node)
  (null (syntax-node-key node)))
(defun syntax-node-children (node)
  (cdr node))
(defun (setf syntax-node-children) (children node)
  (setf (cdr node) children))

(defun walk-syntax-tree (f tree)
  ;; Call F with each node and a list of the parents of the node
  ;; continuing while F returns NIL.  Once it returns non-NIL, return
  ;; its values.  F is allowed to modify the children of the node, but
  ;; not the children of any parent node. If F never returns non-NIL,
  ;; return NIL.
  (iterate walk ((node tree)
                 (parents '()))
    (multiple-value-call
        (lambda (v &rest more)
          (declare (dynamic-extent more))
          (if v
              (return-from walk-syntax-tree
                (apply #'values v more))
            (let ((extended-parents (cons node parents)))
              (dolist (c (syntax-node-children node) nil)
                (walk c extended-parents)))))
      (funcall f node parents))))

(defclass composable-readtable-maker ()
  ;; All composable-readtable-makers should be subclasses of this
  ;; class
  ((dispatching-macro-characters
    :initform '()
    :reader dispatching-macro-characters)
   (dispatching-macro-character-specifications
    :initform '()
    :reader dispatching-macro-character-specifications)
   (macro-character-specifications
    :initform '()
    :reader macro-character-specifications)
   (crm-readtable-case
    :initform (readtable-case *readtable*)
    :reader crm-readtable-case)
   (syntax-tree
    :initform (make-syntax-root-node))
   (special-orders
    :initform '()
    :accessor special-orders)))

(defgeneric add-dispatching-macro-character-to-maker (maker character &key
                                                            non-terminating
                                                            compatible))

(defgeneric add-dispatching-macro-character-specification-to-maker
    (maker character subchar function &key compatible)
  (:method (maker character subchar
                  (function t)
                  &key (compatible nil))
   ;; Get the function definition so other methods can be simpler
   (add-dispatching-macro-character-specification-to-maker
    maker character subchar
    (fdefinition function)
    :compatible compatible)))

(defgeneric add-macro-character-specification-to-maker (maker character function &key
                                                              non-terminating
                                                              compatible)
  (:method (maker character (function t) &key (non-terminating nil) (compatible nil))
   ;; Get the function definition so other methods can be simpler
   (add-macro-character-specification-to-maker maker character
                                               (fdefinition function)
                                               :non-terminating non-terminating
                                               :compatible compatible)))

(defgeneric (setf crm-readtable-case) (new-case maker &key compatible))

(defgeneric add-character-syntax-map-entry-to-maker (maker from to &key compatible))

(defgeneric character-syntax-map (maker))

(defgeneric add-special-order-to-maker (maker order &key compatible)
  (:method (maker (order t) &key (compatible nil))
   (add-special-order-to-maker maker (fdefinition order) :compatible compatible)))

(defmethod add-dispatching-macro-character-to-maker
           ((maker composable-readtable-maker)
            (character character)
            &key (non-terminating nil) (compatible nil))
  (with-slots (dispatching-macro-characters) maker
    (dolist (spec dispatching-macro-characters)
      (when (char= (car spec) character)
        (if compatible
            (unless (or (and (not non-terminating)
                             (not (second spec)))
                        (and non-terminating (second spec)))
              (crm-user-error
               "existing ~A for non-terminating is incompatible with new ~A for ~S"
               (second spec) non-terminating character))
          (setf (second spec) non-terminating))
        (return-from add-dispatching-macro-character-to-maker maker)))
    (push (list character non-terminating) dispatching-macro-characters)
    maker))

(defun add-dispatching-macro-characters-to-maker (maker specs &key (compatible nil))
  (dolist (s specs maker)
    (typecase s
      (character (add-dispatching-macro-character-to-maker maker s
                                                           :compatible compatible))
      ((finite-list-of character t)
       (add-dispatching-macro-character-to-maker maker (first s)
                                                 :non-terminating (second s)
                                                 :compatible compatible))
      (t
       (crm-user-error "bad dispatch char spec ~A" s)))))

(defmethod add-dispatching-macro-character-specification-to-maker
           ((maker composable-readtable-maker)
            (character character)
            (subchar character)
            (function function)
            &key (compatible nil))
  (with-slots (dispatching-macro-character-specifications) maker
    (dolist (spec dispatching-macro-character-specifications)
      (when (char= (car spec) character)
        (dolist (subspec (cdr spec))
          (when (char= (first subspec) subchar)
            (if compatible
              (unless (eql (second subspec) function)
                (crm-user-error "existing function ~A isn't EQL to ~A for ~S / ~S"
                                (second subspec) function character subchar))
              (setf (second subspec) function))
            (return-from add-dispatching-macro-character-specification-to-maker
              maker)))
        (push (list subchar function) (cdr spec))
        (return-from add-dispatching-macro-character-specification-to-maker
          maker)))
    (push (cons character (list (list subchar function)))
          dispatching-macro-character-specifications)
    maker))

(defun add-dispatching-macro-character-specifications-to-maker (maker
                                                                specs
                                                                &key (compatible nil))
  (dolist (s specs maker)
    (typecase s
      ((finite-list-of character character t)
       (add-dispatching-macro-character-specification-to-maker
        maker (first s) (second s) (third s)
        :compatible compatible))
      (t
       (crm-user-error "bad dispatch char spec spec" s)))))

(defmethod add-macro-character-specification-to-maker
           ((maker composable-readtable-maker)
            (character character)
            (function function)
            &key (non-terminating nil) (compatible nil))
  (with-slots (macro-character-specifications) maker
    (dolist (s macro-character-specifications)
      (when (char= (first s) character)
        (if compatible
            (cond
             ((not (eql function (second s)))
              (crm-user-error "existing function ~A isn't EQL to ~A for ~S"
                              (second s) function character))
             ((not (or (and (not non-terminating)
                                 (not (third s)))
                       (and non-terminating
                            (third s))))
              (crm-user-error
               "existing ~A for non-terminating is incompatible with ~A for ~S"
               (third s) non-terminating character)))
          (setf (rest s) (list function non-terminating)))
        (return-from add-macro-character-specification-to-maker maker)))
    (push (cons character (list function non-terminating))
          macro-character-specifications)
    maker))

(defun add-macro-character-specifications-to-maker (maker specs &key
                                                          (compatible nil))
  (dolist (s specs maker)
    (typecase s
      ((finite-list-of character t)
       (add-macro-character-specification-to-maker
        maker (first s) (second s)
        :compatible compatible))
      ((finite-list-of character t t)
       (add-macro-character-specification-to-maker
        maker (first s) (second s)
        :non-terminating (third s)
        :compatible compatible))
      (t
       (crm-user-error "bad macro character spec spec" s)))))

(defmethod (setf crm-readtable-case) (new-case (maker composable-readtable-maker)
                                               &key (compatible nil))
  (unless (member new-case '(:upcase :downcase :preserve :invert))
    (crm-user-error "bad readtable case ~S" new-case))
  (with-slots (crm-readtable-case) maker
    (when (and compatible (not (eql crm-readtable-case new-case)))
      (crm-user-error "incompatible case sensitivity ~S" new-case))
    (setf crm-readtable-case new-case)))

(defmethod character-syntax-map ((maker composable-readtable-maker))
  ;; Return the syntax map for the tree of a crm.  This should be in
  ;; an order in which it can be safely applied.  Entries in the map
  ;; are pairs of (from . to).
  (with-slots (syntax-tree) maker
    (collecting
      (walk-syntax-tree (lambda (node parents)
                          (unless (or (null parents)
                                      (syntax-root-node-p (first parents)))
                            (collect (cons (syntax-node-key (first parents))
                                           (syntax-node-key node))))
                          nil)
                        syntax-tree))))

(defmethod add-character-syntax-map-entry-to-maker
           ((maker composable-readtable-maker)
            (from character) (to character)
            &key (compatible nil))
  ;; This is significantly fiddly.  A node with a given key can appear
  ;; at most once in the tree: it can either appear as a source as a
  ;; child of the root, or as a destination, and possibly also a
  ;; source, lower down.  It is not compatible to change something
  ;; which is only a source into a destination, and it is not
  ;; compatible to change a destination to be a destination of a
  ;; different source.
  ;;
  (when (char= from to)
    (crm-user-error "from and to chars for char syntax entry are the same (~S)"
                    to))
  (with-slots (syntax-tree) maker
    (multiple-value-bind (target parent)
        (walk-syntax-tree
         (lambda (node parents)
           (when (and (not (syntax-root-node-p node))
                      (char= (syntax-node-key node) to))
             (values node (if (and (not (null parents))
                                   (not (syntax-root-node-p (first parents))))
                              (first parents)
                            nil))))
         syntax-tree)
      (cond
       ((not target)
        ;; Destination is not in the tree at all: there can be no
        ;; compatibility issues here.  Look to see if there is a
        ;; source.
        (let ((source (walk-syntax-tree
                       (lambda (node parents)
                         (declare (ignore parents))
                         (when (and (not (syntax-root-node-p node))
                                    (char= (syntax-node-key node) from))
                           node))
                       syntax-tree)))
          (if source
              (push (make-syntax-node to) (syntax-node-children source))
            (push (make-syntax-node from (make-syntax-node to))
                  (syntax-node-children syntax-tree)))))
       ((not parent)
        ;; Target exists, is toplevel.  It is not compatible to move it down.
        (when compatible
          (crm-user-error "would turn ~S into a target of ~S which is incompatible"
                          to from))
        ;; Look for a chain from target to source
        (let ((chain (walk-syntax-tree
                      (lambda (node parents)
                        (when (and (not (syntax-root-node-p node))
                                   (char= (syntax-node-key node) from))
                          (cons node parents)))
                      target)))
          (when chain
            ;; If there's a chain this is bad
            (crm-user-error "would loop ~{~S~^->~}"
                            (cons from (nreverse (mapcar #'syntax-node-key chain))))))
        ;; OK, we can safely move target under a new source
        (setf (syntax-node-children syntax-tree)
              (cons (make-syntax-node from target)
                    (delete target (syntax-node-children syntax-tree)))))
       (compatible
        ;; both target and its parent are found, and we want to be compatible
        ;; which means that parent's key must be from.
        (unless (char= (syntax-node-key parent) from)
          (crm-user-error "existing ~S->~S is incompatible with requested ~S->~S"
                          (syntax-node-key parent) to from to)))
       (t
        ;; both target and its parent are found but compatibility does
        ;; not matter.  First find a possible source and its parents
        (multiple-value-bind (source parents)
            (walk-syntax-tree
             (lambda (node parents)
               (when (and (not (syntax-root-node-p node))
                          (char= (syntax-node-key node) from))
                 (values node parents)))
             syntax-tree)
          (cond ((and source (member target parents))
                 ;; would loop
                 (crm-user-error "would loop ~{~S~^->~}"
                                 (list* from to
                                        (nreverse (mapcar #'syntax-node-key
                                                          (cons source parents))))))
                (source
                 ;; there's a source, would not loop
                 (setf (syntax-node-children parent)
                       (delete target (syntax-node-children parent)))
                 (push target (syntax-node-children source)))
                (t
                 ;; no source: we need a new toplevel source
                 (setf (syntax-node-children parent)
                       (delete target (syntax-node-children parent)))
                 (push (make-syntax-node from target)
                       (syntax-node-children syntax-tree))))))))
    ;; Finally tidy up the tree by removing any toplevel nodes without
    ;; children.  They don't matter, but it's cleaner not to have them
    (setf (syntax-node-children syntax-tree)
          (delete-if (lambda (node)
                       (null (syntax-node-children node)))
                     (syntax-node-children syntax-tree))))
  maker)

(defun add-character-syntax-map-entries-to-maker (maker specs &key (compatible nil))
  (dolist (spec specs maker)
    (add-character-syntax-map-entry-to-maker maker (car spec) (cdr spec)
                                             :compatible compatible)))

#+(and lispworks
       capi
       org.tfeb.tfb)
(defmethod graph-maker-syntax-tree ((maker composable-readtable-maker))
  (capi:contain
   (make-instance 'capi:graph-pane
                  :roots (list (slot-value maker 'syntax-tree))
                  :children-function #'syntax-node-children
                  :print-function (lambda (n)
                                    (if (syntax-root-node-p n)
                                        ""
                                      (format nil "~S" (syntax-node-key n)))))))

(defmethod add-special-order-to-maker ((maker composable-readtable-maker)
                                       (order function) &key
                                       (compatible nil))
  (with-slots (special-orders) maker
    (unless (and compatible (member order special-orders))
      (setf special-orders (nconc special-orders (list order)))))
  maker)

(defun add-special-orders-to-maker (maker orders &key (compatible nil))
  (dolist (order orders maker)
    (add-special-order-to-maker maker order :compatible compatible)))

(defmethod initialize-instance :after
  ((maker composable-readtable-maker)
   &key
   (dispatching-macro-characters nil dmcp)
   (macro-character-specifications nil mcsp)
   (dispatching-macro-character-specifications nil dmcsp)
   (crm-readtable-case nil crcp)
   (character-syntax-map nil csmp)
   (special-orders nil sop))
  (when dmcp
    (add-dispatching-macro-characters-to-maker
     maker dispatching-macro-characters))
  (when mcsp
    (add-macro-character-specifications-to-maker
     maker macro-character-specifications))
  (when dmcsp
    (add-dispatching-macro-character-specifications-to-maker
     maker dispatching-macro-character-specifications))
  (when crcp
    (setf (crm-readtable-case maker) crm-readtable-case))
  (when csmp
    (add-character-syntax-map-entries-to-maker maker character-syntax-map))
  (when sop
    (add-special-orders-to-maker maker special-orders)))

(defun make-composable-readtable-maker
       (&rest keys &key
              (class 'composable-readtable-maker)
              dispatching-macro-characters
              dispatching-macro-character-specifications
              macro-character-specifications
              crm-readtable-case
              character-syntax-map
              special-orders
              &allow-other-keys)
  ;; Should this specify the documented arguments in its arglist?  I
  ;; think doing so helps discoverability.
  "Make a composable readtable maker

The function takes keyword arguments.

DISPATCHING-MACRO-CHARACTERS is a list of dispatching macro
characters.  Each element in the list is either a character or a list
of (character non-terminating).  If it's a character then
non-terminating is false.

DISPATCHING-MACRO-CHARACTER-SPECIFICATIONS is a list of dispatching
macro character specifications.  Each element in the list is a list of
(character subchar function), where character and subchar are the
dispatching macro character and the subcharacter, and function is a
function designator.

MACRO-CHARACTER-SPECIFICATIONS is a list of macro character
specifications, each element of which is either a lisr of (character
function) or a list of (character function non-terminating), with the
first case is equivalent to (character function NIL).  character is a
macro character, function is a function designator.

CRM-READTABLE-CASE is the readable case of the readtable maker.  It
defaults to the readtable case of *READTABLE*.

CHARACTER-SYNTAX-MAP is a list of (from . to) pairs for copying
character syntaxes. Note that this list is somewhat restricted: it
can't contain cycles or anything else which might be order-dependent.
That means, for instance, you can't use it to swap the syntaxes of a
pair of characters: if you want to do that use special orders.

SPECIAL-ORDERS is a list of function designators.
MAKE-READTABLE-FROM-COMPOSABLE-READTABLE-MAKER will call these
functions, in order, with three arguments: the
COMPOSABLE-READTABLE-MAKER object, the source readtable and the target
readtable.  They can do anything they like, and their return value is
ignored.

Note that is is perfectly fine to provide a dispatching macro
character specification for a dispatching macro character not defined
in the object: the readtable will be assumed to already have a
suitable dispatching macro character.  So for instance a specification
like (#\# #\; ...) is almost certainly fine.

In addition the CLASS argument specifies the class of the composable
readtable maker to make, with the default being
ORG.TFEB.COMPOSABLE-READTABLE-MAKER/IMPLEMENTATION:COMPOSABLE-READTABLE-MAKER.

This is simply a wrapper around a call to MAKE-INSTANCE of the
appropriate class: all keyword arguments except CLASS are passed
straight to the MAKE-INSTANCE call.

None of the protocol around subclassing is documented, all of it is
subject to channge."
  (declare (ignorable dispatching-macro-characters
                      dispatching-macro-character-specifications
                      macro-character-specifications
                      crm-readtable-case
                      character-syntax-map
                      special-orders))
  (apply #'make-instance class (remove-properties '(:class) keys)))

(defun compose-composable-readtable-makers (maker &rest to-compose)
  "Compose a composable readtable maker with others

Given one or more composable readtable makers, compose them to return
a new object.

The objects being composed must be compatible:
- dispatching macro characters must agree on non-terminatingness;
- macro character specifications must have functions which are EQL and
  must agree on non-terminatingness;
- dispatching macro character specifications must have functions which
  are EQL;
- the readtable cases must all be the same;
- syntax map entries must be compatible;
- There is no real notion of compatibility for special orders, so the
  lists are simply joined, with each order occuring only once in the
  combined list.  A result of this rule is that the order of any two
  special orders will always be the same.

If they are not compatible a CRM-USER-ERROR will be
signaled."
  (let ((new (make-composable-readtable-maker
              :class (class-of maker)
              :dispatching-macro-characters
              (dispatching-macro-characters maker)
              :dispatching-macro-character-specifications
              (dispatching-macro-character-specifications maker)
              :macro-character-specifications
              (macro-character-specifications maker)
              :crm-readtable-case (crm-readtable-case maker)
              :character-syntax-map (character-syntax-map maker)
              :special-orders (special-orders maker))))
    (dolist (c to-compose new)
      (add-dispatching-macro-characters-to-maker
       new (dispatching-macro-characters c) :compatible t)
      (add-dispatching-macro-character-specifications-to-maker
       new (dispatching-macro-character-specifications c) :compatible t)
      (add-macro-character-specifications-to-maker
       new (macro-character-specifications c) :compatible t)
      (setf (crm-readtable-case new :compatible t)
            (crm-readtable-case c))
      (add-character-syntax-map-entries-to-maker
       new (character-syntax-map c) :compatible t)
      (add-special-orders-to-maker new (special-orders c) :compatible t))))

(defgeneric make-readtable-from-composable-readtable-maker
    (maker &key from to)
  (:documentation "Make a readtable from a composable readtable maker

MAKER is the composable readtable maker.

FROM & TO have the same semantics as for COPY-READTABLE, and in
particular if FROM is omitted it defaults to the dynamic value of
*READTABLE*, and if TO is given then the returned readtable will not
be new.

There may be other keyword arguments if MAKER is not the default
composable readtable maker class: none of this protocol is documented
yet.")
  (:method :around (maker &key (from *readtable*) (to nil))
   ;; Default the readtables properly so all the methods don't need to
   ;; do it
   (call-next-method maker :from from :to to)))

(defmethod make-readtable-from-composable-readtable-maker
           ((maker composable-readtable-maker)
            &key from to)
  (let ((rt (copy-readtable from to)))
    (dolist (dmc (dispatching-macro-characters maker))
      (destructuring-bind (character non-terminating) dmc
        (make-dispatch-macro-character character non-terminating rt)))
    (dolist (dmc-spec (dispatching-macro-character-specifications maker))
      (destructuring-bind (character . subspecs) dmc-spec
        (dolist (subspec subspecs)
          (destructuring-bind (subchar function) subspec
            (set-dispatch-macro-character character subchar function rt)))))
    (dolist (mc-spec (macro-character-specifications maker))
      (destructuring-bind (character function non-terminating) mc-spec
        (set-macro-character character function non-terminating rt)))
    (setf (readtable-case rt) (crm-readtable-case maker))
    (dolist (csm-entry (character-syntax-map maker))
      (set-syntax-from-char (cdr csm-entry) (car csm-entry)
                            rt from))
    (dolist (special-order (special-orders maker))
      (funcall special-order maker from rt))
    rt))

;;;; Some tests for the syntax copying stuff
;;;

(macrolet ((should-error (form &optional (message "should error but ~@{~S~^ ~}"))
             ;; Message gets arguments which are results of form
             `(handler-case ,form
                (crm-user-error ()
                  (values))
                (crm-error (e)
                  (warn "oops: ~A" e))
                (:no-error (&rest vals)
                  (apply #'warn ,message vals))))
           (should-not-error (form &optional (message "should not error, but ~A"))
             ;; message argument is condition
             `(handler-case ,form
                (crm-user-error (e)
                  (warn ,message e))
                (crm-error (e)
                  (warn "oops: ~A" e)))))
  (flet ((mcsm (&optional (csm '()))
           (make-composable-readtable-maker
            :character-syntax-map csm)))
    (should-error
     (mcsm '((#\x . #\x)))
     "direct loop undetected in ~S")
     (let ((m (mcsm)))
       (should-not-error
        ;; no destinationm no loops
        (add-character-syntax-map-entry-to-maker m #\x #\y)
        (add-character-syntax-map-entry-to-maker m #\y #\z)))
     (let ((m (mcsm '((#\x . #\y)))))
       (should-error
        ;; moving toplevel down is not compatible
        (add-character-syntax-map-entry-to-maker
         m #\z #\x :compatible t)
        "incompatibly moving toplevel down in ~S")
       (should-not-error
        (add-character-syntax-map-entry-to-maker
         m #\z #\x))
       (should-error
        ;; check #\z is now the top
        (add-character-syntax-map-entry-to-maker
         m #\g #\z :compatible t)
        "failed sanity check")
       (should-error
        ;; This should fail as #\y is down the tree
        (add-character-syntax-map-entry-to-maker
         m #\y #\z)
        "loop undetected from toplevel in ~S"))
    (let ((m (mcsm '((#\x . #\y)))))
      (should-error
       ;; incompatible move
       (add-character-syntax-map-entry-to-maker
        m #\z #\y :compatible t)
       "incompatible move unfound")
      (should-not-error
       (add-character-syntax-map-entry-to-maker
        m #\z #\y)
       "failed to move when should"))
    (let ((m (mcsm '((#\x . #\y) (#\y . #\z)))))
      (should-error
       (add-character-syntax-map-entry-to-maker
        m #\q #\y :compatible t))
      "incompatible move (sanity)"
      (should-not-error
       (add-character-syntax-map-entry-to-maker
        m #\q #\y :compatible nil)
       "incompatible move (sanity)")
      (should-error
       (add-character-syntax-map-entry-to-maker
        m #\z #\y)
       "loop undetected"))))
