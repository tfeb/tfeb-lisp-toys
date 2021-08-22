;;;; Classes with hidden slots
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.collecting
 :org.tfeb.hax.iterate)

(defpackage :org.tfeb.toys.hidden-slots
  (:use :cl)
  (:use :org.tfeb.hax.collecting :org.tfeb.hax.iterate)
  (:export
   #:hidden-slot-class-names
   #:clear-hidden-slots-for-class-name
   #:defclass/hs
   #:with-hidden-slots
   #:slot-name-for-instance
   #:slot-name-for-class))

(in-package :org.tfeb.toys.hidden-slots)

(provide :org.tfeb.toys.hidden-slots)

(defvar *class-hidden-slot-map* '())

(defun hidden-slot-class-names ()
  "Return a list of the names of classes with hidden slots"
  (mapcar #'car *class-hidden-slot-map*))

(defun clear-hidden-slots-for-class-name (class-name)
  "Clear the hidden slots for CLASS-NAME

If it is not a known class with hidden slots return NIL, otherwise
return its name."
  (let ((found (assoc class-name *class-hidden-slot-map*)))
    (if found
        (progn (setf (cdr found) '())
          class-name)
      nil)))

(defun hidden-slot-name-for-class-name (slot-name class-name friend-names)
  (iterate next
      ((this-map (cdr (or (assoc class-name *class-hidden-slot-map*)
                          (cdr (car (push (cons class-name nil)
                                          *class-hidden-slot-map*))))))
       (more-names friend-names))
    (or (cdr (assoc slot-name this-map))
        (If (not (null more-names))
            (destructuring-bind (next-name . more) more-names
              (next (cdr (or (assoc next-name *class-hidden-slot-map*)
                             (cdr (car (push (cons next-name nil)
                                             *class-hidden-slot-map*)))))
                    more))
          (let ((name (make-symbol (concatenate 'string
                                                (symbol-name slot-name)
                                                "/"
                                                (symbol-name class-name)))))
            (push (cons slot-name name)
                  (cdr (assoc class-name *class-hidden-slot-map*)))
            name)))))

(defun rewrite-slot-specifications (specs class-name friend-names)
  (mapcar (lambda (spec)
            (etypecase spec
              (symbol
               (hidden-slot-name-for-class-name spec class-name friend-names))
              (cons
               (cons (hidden-slot-name-for-class-name (first spec)
                                                      class-name friend-names)
                     (rest spec)))))
          specs))

(defun find-friend-names (class-options)
  (with-collectors (friend-name remaining-option)
    (dolist (option class-options)
      (case (first option)
        ((:friends)
         (dolist (friend-name (rest option))
           (friend-name friend-name)))
        (otherwise
         (remaining-option option))))))

(defmacro defclass/hs (class supers slots &rest options)
  "Like DEFCLASS, but slots are hidden

Slot names are turned into uninterned symbols.  The mapping between
the provided slot names and the real names is maintained so that
redefining a class works, and so that friends can see them.

The class options can include (:friends ...) which will say that the
named classes are friends of this class, and their hidden slot names
will be mapped by this class too.  Note that this is a unilateral
assertion: a class definition doesn't say 'I will allow these other
classes to be my friends' but rather 'I am friends with these other
classes'.  It's up to you to ensure whether the classes a class wants
to be friendly with are OK with this.  Forward-references to friends
won't generally work because the friend's slot names are not yet
known."
  (multiple-value-bind (friend-names remaining-options) (find-friend-names options)
    (setf (get class 'friend-names) friend-names)
    `(defclass ,class ,supers
       ,(rewrite-slot-specifications slots class friend-names)
       ,@remaining-options)))

#+LispWorks
(editor:setup-indent "defclass/hs" 2)

(defun slot-name-for-instance (instance slot-name)
  "Get the slot name corresponding to SLOT-NAME for OBJECT

See SLOT-NAME-FOR-CLASS, which this immediately calls"
  (slot-name-for-class (class-name (class-of instance)) slot-name))

(defgeneric slot-name-for-class (class/name slot-name)
  (:method ((class class) slot-name)
   (slot-name-for-class (class-name class) slot-name))
  (:documentation  "Return the slot name for a class or class name

A thing may be: a class, or the name of a class.  It makes no check
that the slot exists.  The only case this *won't* return SLOT-NAME is
when a class has hidden slots."))

(defmethod slot-name-for-class ((class-name symbol) (slot-name symbol))
  (iterate next ((this class-name)
                 (more (get class-name 'friend-names '())))
    (or (cdr (assoc slot-name
                    (cdr (assoc this *class-hidden-slot-map*))))
        (and (not (null more))
             (next (first more) (rest more)))
        slot-name)))

(defmacro with-hidden-slots (bindings &body forms)
  "A variant of WITH-SLOTS which supports hidden slots.

Each binding is one of:
- a symbol which is the slot name;
- a list of (name class-name-which-may-be-hiding-name) which will
  excavate the hidden name from the named class and turn into a suitable
  binding of name to it;
- a list of ((name slot-name) class-name) which does the same but
  binds name to the escavated value of slot-name.

It's fine for the slot not to be hidden: it's just used as is then.
Expands to WITH-SLOTS in a fairly obvious way."
  `(with-slots ,(mapcar
                 (lambda (binding)
                   (typecase binding
                     (symbol binding)
                     (list
                      (unless (and (= (length binding) 2))
                        (error "bad slot binding ~S" binding))
                      (multiple-value-bind (variable slot-name class-name)
                          (typecase (first binding)
                            (symbol
                             (values (first binding) (first binding)
                                     (second binding)))
                            (list
                             (unless (= (length (first binding)) 2)
                               (error "bad slot binding ~S" binding))
                             (values (first (first binding))
                                     (second (first binding))
                                     (second binding)))
                            (t
                             (error "hopeless slot binding ~S" binding)))
                        `(,variable ,(slot-name-for-class class-name slot-name))))
                     (t
                      (error "mutant slot binding ~S" binding))))
                 bindings)
       ,@forms))

#||
(defclass/hs foo ()
  ((x :accessor foo-x)))

(defclass/hs bar ()
  ((x :accessor bar-x)
   (y :accessor bar-y))
  (:friends foo))

(defclass bone (foo bar)
  ())

(defclass/hs fish ()
  ((y :accessor fish-y)
   (x :accessor fish-x))
  (:friends bar))

(defclass/hs batbox (fish bar)
  ;; This will have two variants of the X slot, one from FISH and one
  ;; from FOO via BAR, and a single Y slot from BAR via FISH.
  ())
||#
