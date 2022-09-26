# [TFEB.ORG Lisp toys](https://github.com/tfeb/tfeb-lisp-toys "TFEB.org Lisp toys")
This repo contains a collection of small Common Lisp toys I've written over the last thirty-odd years.  These are explicitly *toys*: they're little things which I wrote for amusement and which I decided might one day be useful either to me or someone else.

These toys may make their way into a more formal place, such as my [Lisp hax repo](https://github.com/tfeb/tfeb-lisp-hax "TFEB.ORG Lisp hax").  When that happens they'll vanish from here (perhaps with a note).  There will therefore never be releases from this repo and you should never assume stability of what's here.  These are just, well, toys.

The documentation is generally rather sparse, and it’s also just in the order I wrote it.  Some of the toys may work, some may not.  Some may never have worked.

## General
### Modules
All of these things are independent modules, providing something which looks like `org.tfeb.toys.<module>`.  There is no ASDF or other system definition for all of them.  If they need other things either from amongst themselves or from other modules I've written they'll do so using [`require-module`](https://github.com/tfeb/tfeb-lisp-tools#requiring-modules-with-searching-require-module "require-module") which you'll need to have installed in that case, or if you don't to have loaded those modules some other way.  In the case that they need things written by other people or other larger systems they'll do that using [Quicklisp](https://www.quicklisp.org/ "Quicklisp").  Some of them do the former, none currently do the latter.

### Portability
All of the toys purport to be portable Common Lisp.

### Zero history
The repo from which the toys are published was invented in 2021, but some of them are much older than that.  Some of that history exists but not in the publication repo.

### Naming conventions
All of the toys use *domain-structured names*: packages, modules, features and so on have names which start with a reversed DNS domain and then continue to divide further.  The prefix for all of the toys is `org.tfeb.toys`.  See [the TFEB.ORG tools documentation](https://github.com/tfeb/tfeb-lisp-tools#naming-conventions "TFEB.ORG tools / Naming conventions") for a little more on this.  If they move elsewhere, these names will change.

### The likely fate of some of the toys
`locatives` will likely end up in my hax repo, as will `fluids`.  `glex` I'm not sure about.

`simple-loops`, `metatronic` and `slog`, which formerly lived here, have now migrated to the hax repo.

---

## An iteration protocol: `for`
This lets you define iterators for objects by defining methods on `iterator` which typically returns a function.  `next` (another generic function) should then know how to get the next element from an iterator: for an iterator which is a function it just calls it with itself as an argument.  `for` is a macro which will loop over objects, calling `iterator` to make the iterator & `next` to get the next values.  Finally there is a `gather` macro which is a bit like Python's list comprehensions (or what I thought they were like in 2004).

As an example a simplified method on `iterate` for lists could be:

```lisp
(defmethod iterator ((l list) &key)
  (lambda (self)
    (if l
        (values
         (prog1 (first l)
           (setf l (rest l)))
         self)
      (values nil exhausted-iterator))))
```

And you could then gather the elements of a list which are odd numbers like this:

```lisp
(gather (* x x)
  for (x '(1 2 t 3))
  when (numberp x)
  when (oddp x))
```

Note that `for` & `when` are not `loop`-style keywords: `gather` turns them into `(when ...)`, so they're just the normal CL macros[^1]:

```lisp
(gather (* x x)
  for (x '(1 2 t 3))
  when (numberp x)
  when (oddp x))
 -> (let ((#:a 'nil) (#:at 'nil))
      (for (x '(1 2 t 3))
        (when (numberp x)
          (when (oddp x)
            (if #:a
                (setf (cdr #:at) (list (* x x)) #:at (cdr #:at))
              (setf #:a (list (* x x)) #:at #:a)))))
      #:a)
```

`for` lives in `org.tfeb.toys.for` and provides `:org.tfeb.toys.for`.

I fairly heavily revised `for` in 2022: the protocol iterators support has partly changed,  the function formerly known as `iter` is now`iterator`, and there have been a number of other significant changes: the new version is not particularly compatible with the old one.

## Turning `case` into an explicit jump table: `ncase`
`ncase` is just like `case`but it can turn `case` statements into explicit jump tables if they are large enough.  From the comments in the file:

> This is actually not a really good example because the compiler is likely to be able to do a better job of the decisions involved, and it can also use real jumps rather than function calls which only may turn into jumps.

I can't see a case where `ncase` would be useful today, but it's there.

`ncase` lives in `org.tfeb.toys.ncase` and provides `:org.tfeb.toys.ncase`.

## Global lexical variables: `glex`
CL only has global *special* variables, which are dynamically scoped.  But it's easy to fake global variables which are lexically scoped using symbol macros.  That's what `glex` does.

**`defglex`** is like `defvar`: `(defglex x)` will define `x` as a global lexical but not give it an initial value, while `(defglex x 1)` will give it an initial value as well.  You can provide documentation strings.

**`defglex*`** is like `defparameter`: you can't omit the value and it is set each time.

**`make-glex-readtable`\*\* will construct a readtable in which `#$x` refers to a global lexical variable `x`.  It has three keyword arguments:

- `from` is the readtable to copy, defaultly `*readtable*`;
- `to` is the readtable to copy into, defaultly `nil`;
- `dollar` is the subcharacter to use, defaultly `#\$`.

Example:

```lisp
> (setf *readtable* (make-glex-readtable))
#<readtable 4020064633>

> (setf #$x 1)
1

> #$x
1

> (defglex x)
x

> x
1
```

`glex` lives in `org.tfeb.toys.glex` and provides `:org.tfeb.toys.glex`.

## Racket-style parameters: `parameters`
This lets you wrap special variables in functions and provides forms which will 'bind' these named functions.

**`define-parameter`** is like `defvar`.  **`define-parameter*`** is like `defparameter`.  **`parameterize`** is like `let`, **`parameterize*`** is like `let*`

An example:

```lisp
(define-parameter hugeness 10 "how huge a thing is")

(print (hugeness))
(parameterize ((hugeness 20))
  (print (hugeness)))
```

will print `10` and then `20`.

`parameters` lives in `org.tfeb.toys.parameters` and provides `:org.tfeb.toys.parameters`.

See also fluids, below.

## Composable readtable makers: `composable-readtable-maker`
Something I often find myself wanting to do is to make two or more changes to a readtable with the intention that these changes are orthogonal to each other.  Without writing special code each time this is hard to ensure.  Solving this problem is what composable readtable makers are for.

A *readtable maker* is an object which encapsulates a set of changes to make to a readtable – it's almost but not quite a readtable factory[^2].  A *composable* readtable maker is a readtable maker which can be composed with another readtable maker to either produce a readtable maker which will be compatible with both, or signal an error if that is not possible.

Composing composable readtable makers produces a composable readtable maker which is compatible with all of them.  However, making a readtable from this object may still change it in incompatible ways: that problem isn't something I tried to solve.

Composable readtable makers are almost but not quite simple to implement.  The bit which isn't simple is also the thing that limits them: copying character syntax.  This is fiddly because it can be order-dependent in general and because you might want to do things such as swap the syntaxes of two characters.  So this is all avoided: you can only specify how to copy character syntax in a way which is both loop free and order-independent.  Checking this is mildly fiddly.

To deal with limitations like those that apply to character syntax, there are also 'special orders' which are simply lists of functions which get called.

## Hidden slots: `hidden-slots`
Some people want slots in classes defined with `defclass` to be private to those objects.  There is, of course, nothing stopping you saying that they are: CLOS is best regarded as a system for *writing* object systems.  If you want things to look like a message-passing system then CLOS can let you do that, if you want slots to be private then CLOS can do that.  In the presence of macros and an effectively-standard MOP you can construct whatever system suits you as *The Art of the Metaobject Protocol* makes very clear: CL is a language which assumes its users are humans, not sheep.  What's more, CL is so powerful and expressive that these systems can be written in minutes, not hours or days: `hidden-slots` took well under an hour to write in its initial, fully-functional version.

`hidden-slots` provides a macro, **`defclass/hs`**, which lets you make slots private to the class where they are defined.  `defclass/hs` is exactly like `defclass` except that all slot names are rewritten to be gensyms:

```lisp
(defclass/hs foo ()
  ((x ...)))

(defclass/hs bar (foo)
  ((x ...)))
```

will result in instances of `bar` having two slots, not one.  `defclass/hs` however lets a class say that it is a 'friend' of one or more other classes, which means it can see their secret slot names:

```lisp
(defclass/hs foo ()
  ((x ...)))

(defclass/hs bar (foo)
  ((x ...))
  (:friends foo))
```

will result in instances of `bar` having only one slot.  Friendship is unilateral: a class can't say who it will allow to be its friends[^3].  Note that friendship has nothing to do with inheritance:

```lisp
(defclass foo ()
  ((x ...)))

(defclass bar ()
  ((x ...))
  (:friends foo))

(defclass bone (foo bar)
  ())
```

will cause instances of `bone` to have one slot which will unify the slots of `foo` and `bar`.

All this is done based on class *names*: there's no fancy MOP stuff here.

One consequence of hidden slots is that you can't, for instance, know if a slot is bound in an instance, or easily poke around without using MOP functionality to get the slots which exist.  Well, sometimes you need to do things like that, so there are some tools to let you find out about hidden slots: **`slot-name-for-instance`** returns the slot name for an instance corresponding to the name you give; **`slot-name-for-class`** returns the slot name for a class (you need both of these because you might want to define metaclasses with hidden slots).  Neither of these check if the slot actually exists: they will remap hidden slot names but for any other slot name they'll just return the name.  **`with-hidden-slots`** is a variant of `with-slots` which will remap slot names (you need to tell it the class name to remap for, of course).

Finally there are two maintenance functions: **`hidden-slot-class-names`** tells you the names of classes which have hidden slots, and **`clear-hidden-slots-for-class-name`** will cause that class name to forget its hidden slot mapping (which, until you redefine the class, means that its slots now really are irrevocably hidden unless you use the MOP to find them).

It does not work to say you are the friend of a class which is not yet defined, because its name mappings don't yet exist.

`hidden-slots` is explicitly a toy: I wrote it just as an example of how easy it is to do something like this.  Something like it might be fine as part of a program, but I don't think it's library-quality (and still less language-quality) code.

## Dynamic bindings for fields: `fluids`
Programmers in languages which don't have dynamic variables inevitably have to invent them, either by some horrible non-thread-safe shallow-binding approach or more nicely.   Quite often, of course, people writing in such languages don't even *know* they're reinventing dynamic variables: such is the state of education in computing.

Python doesn't have dynamic variables, but it does have enough portable mechanism to invent them, with a thread-safe deep-binding approach.  A while ago when I was writing Python for my living I did that, in a module called `nfluids`[^4], which I should get around to publishing.  Fluids in this implementation were functions which looked up their value on a secretly maintained, thread-local, binding stack which was maintained by `with ...` constructs, thus giving you deep binding.

A nice feature of this approach is that it solves the 'dynamically bind a field/slot in an object' problem: a fluid doesn't change, but the binding it accesses does, since the fluid is essentially a key into the binding stack.  And in a Lisp-1, like Python, it's also pretty elegant.

Well, although CL has dynamic variables, the dynamic-slot-binding problem still exists.  Here's an example.  Given

```lisp
(defstruct foo
  (x 1))
```

let's imagine I have a `foo` and I want, dynamically, to bind `foo-x` to `2`.  Here's a horrible approach to doing that:

```lisp
(let ((old-x (foo-x it)))
  (unwind-protect
      (progn (setf (foo-x it) 2) ...)
    (setf (foo-x it) old-x)))
```

This is terrible because it's not thread-safe, and, in any implementation with real concurrency it can't be made so.

Here's how this would look with fluids:

```lisp
(defstruct foo
  (x (make-fluid :value 1)))

(fluid-let (((foo-x it) 2))
  ...
  (fluid-value (foo-x it))
  ...)
```

This is less pretty than the implementation in a Lisp-1: even if fluids were functions (which they're not, in this implementation) you'd still need some equivalent of `fluid-value` (which might just be `funcall`) in a Lisp-2.  But it is thread-safe because, of course, it secretly uses a dynamic variable on which the dynamic bindings of fluids are consed.

There are several possible obvious implementations of fluids: all a fluid really needs to be is a unique object which can serve as a key into the binding stack, and also which can store a global value somehow.  Two obvious implementations are symbols (keeping the global value in `symbol-value` or on the property list of the symbol) or conses, keeping the global value in the `cadr` of the cons (the reason for this is so it's possible for the fluid not to be bound, which I wanted).  Both of these implementations have the problem (which my Python implementation using functions did have) that you can't easily write a `fluidp` predicate, which I wanted.  So this implementation uses a CLOS object for fluids, relying on the ability of its single slot not to be bound to support unbound fluids.  There's an implementation which uses conses in the history of the repo.

Here is a rough description of the interface.

**`make-fluid`** makes a fluid.  It has two keyword arguments:

- `value` will give the fluid a global value – without it the fluid will be globally unbound (unless it got its value from `from`);
- `from` will copy a fluid, which just means copying its global (not local!) value if any.

**`fluidp`** tells you if an object is a fluid.

**`fluid-value`** accesses the binding of a fluid:

- `(fluid-value f)` will retrieve the current dynamically-apparent binding of `f` or signal an error if it is unbound;
- `(setf (fluid-value f) ...)` will set the binding, and will signal an error if the fluid is unbound;
- `(fluid-value f t)` will retrieve the global binding of `f`, and will signal an error if there is no global binding;
- `(setf (fluid-value f t) ...)` will set the global binding, and will succeed even if there is no current global binding.

The second argument does not need to be `t`: it just needs to be not `nil` as you'd expect.  This is true below as well.

**`fluid-boundp`** tells you things about whether a fluid is bound.  It is slightly complicated.

- `(fluid-boundp f)` returns two values: whether a fluid is bound at all, and whether it has a global binding.  Not all possibilities can happen, since if a fluid has is not bound it can't have a toplevel binding (see below).
- `(fluid-boundp f t)` returns two values: whether a global binding exists and whether a dynamic binding exists.  In this case all the possibilities *can* happen.

**`fluid-makunbound`** makes a fluid be globally unbound.  Although it's possible to construct an implementation where a fluid is *locally* unbound but *globally*bound by having a special 'not bound' value which can be the value of a local binding (and it is at least arguable that this can happen in CL with `makunbound` and special variables, see the discussion [here](http://cl-su-ai.lisp.se/msg03748.html "dynamic bindings and MAKUNBOUND")), I decided that such an approach was perverse, so fluids don't support that.  So `fluid-makunbound` will succeed only if there is no local binding for the fluid and wil signal an error otherwise.  This is why `fluid-boundp` can't return all the possibilities, of course.

**`define-fluid`** is just a wrapper around `defvar` which defines a fluid as a global variable.  I'm not sure it should exist, but it does.

**`call/fluid-bindings`** calls its first argument with the fluids and values which are its remaining values bound.  Example:

```lisp
(let ((f (make-fluid)))
  (call/fluid-bindings (lambda () (fluid-value f)) f 3))
```

will evaluate to `3`.

**`fluid-let`** is a macro version of `call/fluid-bindings`:

```lisp
(let ((f (make-fluid :value 4)))
  (fluid-let ((f 5))
    (fluid-value f)))
```

evaluates to `5`.  Note that `fluid-let` evaluates both parts of each pair of fluid & value:

```lisp
(let ((c (cons (make-fluid) nil)))
  (fluid-let (((car c) 5))
    (fluid-value (car c))))
```

does what it looks like.

**`fluid-let*`** is to `fluid-let` what `let*` is to `let`.

**`fluid-error`** is the class of error conditions related to fluids: `fluid-error-fluid` will retrieve the offending fluid.

**`unbound-fluid-error`** is a subclass of `fluid-error` which is signalled when a fluid is unexpectedly unbound.

**`bound-fluid-error`** is a subclass of `fluid-error` which is signalled when a fluid is unexpectedly bound: this only happens when you try and call `fluid-makunbound` on a fluid with a local binding.

`fluids` lives in `org.tfeb.toys.fluids` and provides `org.tfeb.toys.fluids`.  It is probably on its way to becoming something more than a toy (when its package will change).

## Making variables inaccessible: `descope`
I quite often find I make mistakes like the following when I’m taking a bit of code which uses some variable and modifying it to use something derived from it, but then forgetting to replace all the references to the outer variable:

```lisp
(defun foo (l)
  (let ((l1 (function-of l)))
    ...
    (let ((l2 (another-function-of l1)))
      ... update l2 ...
      ... (when (null l1) (return ...)) ...)))
```

So I wrote a little macro which makes it possible to say ‘these variables are now out of scope’.

**`descoping`** is a macro in the body of which one or more variables are out of scope:

```lisp
(let ((y (f x)))
  (descoping (x)
    ... references to x are now errors ...))
```

It does this the way you would expect: it binds the symbols in its first arguments as symbol-macros which create a compile-time warning and a run-time error.  The warning is a `descoped-variable-warning` and the error is a `descoped-variable-error` and they’re both subclasses of `descoped-variable`.

I used this exactly once to help me fix idiocies in a program I was modifying: I think it’s only real use is to demonstrate just how much you make make the language do for you.

It is in `org.tfeb.toys.descope` and provides `org.tfeb.toys.descope`.

## Spaghetti code: `spaghetti`
> procedure calls may be usefully thought of as GOTO statements which also pass parameters – Guy Steele, [Lambda: the ultimate GOTO](https://dspace.mit.edu/handle/1721.1/5753 "Lambda: the ultimate GOTO")

If you use `spaghetti` you do actually have a GOTO which passes arguments.  It has constructs, `labelling` and  `labelling*`, within which you can define labels with forms like `(label name [arguments])` and `(label* name [arguments])`: 'invoking' a label consists of assigning values corresponding to its arguments, if any, and then jumping to the place it's defined.  Here's an example:

```lisp
(labelling ((i 0))
  (label top (i))
  (print i)
  (when (< i 10)
    (top (1+ i)))
  i)
```

Here `top` is  label, and `(top x)` jumps to that label, assigning a value to `i`.  This can be made more concise (perhaps opaque would be a better word):

```lisp
(labelling ((i 0))
  (label top (&optional (i (1+ i))))
  (print i)
  (when (< i 10)
    (top))
  i)
```

In this form the label defined by `top` has an optional argument and will increment `i`if it is not given.  So you can easily write this horrid thing:

```lisp
(defun foo (n m)
  (labelling ((i 0) (j 0))
    (label top (&key (i (1+ i)) (j j)))
    (when (= i m)
      (top :i 0 :j (1+ j)))
    (when (= j n)
      (bottom))
    (format t "~&i=~D j=~D~%" i j)
    (top)
    (label bottom)
    (values)))
```

And now:

```lisp
> (foo 2 2)
i=0 j=0
i=1 j=0
i=0 j=1
i=1 j=1
```

Why you would *want* to write this escapes me, but you could.

**`labelling`** is a form which binds zero or more variables the way `let` does, and the body of which is an implicit `progn` except that the  `label` form may be used to establish labels.  It establishes an implicit `block` named `nil` so  `return` `return-from` will escape from it.  Declarations are allowed at the start of the body.

**`labelling*`** is like `labelling` except that the initial bindings are done sequentially, like `let*`.

**`label`** is a form which can be used only within `labelling` / `labelling*`.  It has two syntaxes:

`(label name)` extablishes `name` as a label, and then `(name)` will jump to that label.

`(label name arglist)` establishes `name` as a label, but 'invoking' name now will perform a number of assignments to the variables named in the arglist before the jump.  These assignments happen in parallel (like `psetq`).   You can use `&optional`, `&key` and `&aux` in the arglist: `&aux` is useful when you want to have an assignment which can't be overridden.  No other lambda-list keywords are allowed.

It's probably easiest to explain what happens with a couple of examples.

- Given `(label foo (x y))` then `(foo 1 2)` corresponds to `(progn (psetq x 1 y 2) (go foo)`.
- Given `(label foo (&optional (i (1+ i)))` then
	- `(foo)` corresponds to `(progn (psetq i (1+ i)) (go foo))`;
	- `(foo 1)` is `(progn (psetq i 1) (go foo))`.
- Given `(label foo (&key (x x) (y 0))` then the assignments happen to the default values specified or to the arguments given, if any.
- Given `(label foo (&aux (i (1+ i))))` then `(foo)` will always increment `i`.

**`label*`** is the same as `label` except that assignments ae done sequentially: with `setq` rather than `psetq`.  The order of the assignments depends on their order in the arglist, *not* the order they are given in when invoking the label: this can be different for keyword arguments.   So, for example

```lisp
(labelling ((a 1) (b 2))
  (end)
  (label end (&aux (a (1+ 1)) (b (1+ a))))
  (values a b))
```

evaluates to `2` and `2`, while

```lisp
(labelling ((a 1) (b 2))
  (end)
  (label* end (&aux (a (1+ 1)) (b (1+ a))))
  (values a b))
```

evaluates to `2` and `3`.

Notes.

- The things defined by `label` are macros, not local functions.  That means, for instance, keyword argument processing happens at macroexpansion time and will have no runtime cost at all.
- Default value forms are spliced in literally as you'd expect for macros.
- The variables assigned to don't have to be those defined by `labelling` / `labelling*`.
- `label*` is probably more natural (and perhaps more efficient) than `label`while having a less natural name.  But the analogy with `let*` / `let` was too close for me to want to swap them.
- May explode without warning, especially if you use it.  Dangerous to fish.

It is in `org.tfeb.toys.spaghetti` and provides `org.tfeb.toys.spaghetti`.

## Square-bracket forms: `sb-readtable`
CL, being a Lisp-2, makes it somewhat verbose to pass functions around as arguments: you end up with a lot of `(funcall x ...)`s in code.  `sb-readtable` makes it so that `[x ...]` is read as `(funcall x ...)`, and it can do other things as well.  You can in fact control what the pair of bracket characters are if you want to do that.

The readtable constructed by `make-sb-readtable` lets you read forms like `[...]`.    Normally `[...]` reads as `(<op> ...)` where `<op>` is the value of `*sb-operator-name*`. (which by default is `funcall`).  However, if `*sb-transformer*` is not `nil`, then it is used to transform the form read into anything it wants to.  Note that both of these things happen at read time.

**`make-sb-readtable`** returns a readtable in which `#\[` (or any character you specify) is defined to read a form as described above, and `#\]` (or any other character you specify) is an error.  It has three keyword arguments:

- `brackets` should be a list of an open and close bracket character, and is by default `(#\[ #\])`;
- `from-readtable` is the readtable to copy, defaultly `*readtable*`
- `to-readtable` is the readtable to copy into, defaultly `nil`.

`from-readtable` and `to-readtable` have the same semantics as for `copy-readtable`.

**`*sb-operator-name*`** is the operator name interposed by default in `[...]` syntax.  Defaultly it is `funcall`.

**`*sb-transformer*`** is either `nil` or a designator for a function of two arguments: the form read by `[...]` and the stream it was read from.  Its result is used as the value of the read form.  It's passed the stream so it can signal useful read-time errors.  Default value is `nil`.

`sb-readtable` lives in `org.tfeb.toys.sb-readtable` and provides `org.tfeb.toys.sb-readtable`.

## Not really locatives: `locatives`
Zetalisp had special things called *locatives* which were a bit like pointers.  I can't remember how they worked but they needed hardware support I am fairly sure.  What this code does is invent an object called a 'locative', constructed with the `locative` macro, which encapsulates a reference to a place.  `locf` will then retrieve the value of that place and `(setf locf)` will store a value in it.  As an example:

```lisp
> (let* ((x (list 1 2 3))
         (loc (locative (cdr x))))
    (locf loc))
(2 3)

> (let* ((x (list 1 2 3))
         (loc (locative (cdr x))))
    (setf (locf loc) 4)
    x)
(1 . 4)
```

This works quite generally: given

```lisp
(defun foo ()
  (let ((y 8))
    (bar (locative y))
    y))

(defun bar (loc)
  (setf (locf loc) 3))
```

then

```lisp
> (foo)
3
```

**`locative`** creates a locative to a place.

**`locf`** retrieves the value of the place referenced by a locative.

**`(setf locf)`** updates the value of the place referenced by a locative.

**`with-locatives`** wraps some symbol macros around locatives to make them look like variables.

As an example of `with-locatives`, `bar` above could be written as:

```lisp
(defun bar (loc)
  (with-locatives ((l loc))
    (setf l 3)))
```

or, if you don't want a new name for the locative:

```lisp
(defun bar (loc)
  (with-locatives (loc)
    (setf loc 3)))
```

Locatives are, of course, implemented by functions which capture the appropriate lexical scope where they are created.  They have indefinite extent as you would expect, although they're most often useful.

`locatives` lives in `org.tfeb.toys.locatives` and provides `org.tfeb.toys.locatives`.  It will probably end up in my hax repo.

## Decorators
Python has [a syntax which allows you to 'decorate' definitions](https://docs.python.org/3/reference/compound_stmts.html#function-definitions "Python decorators"):

```python
@f1(arg)
@f2
def foo (x):
    return x
```

is equivalent to

```python
def foo (x):
    return x
foo = f1(arg)(f2(foo))
```

(Note that Python is a Lisp-1, so this makes sense.)

`decorators` lets you do this, but it is in fact far more general: it lets you rewrite things in completely arbitrary ways.  So, for instance, here is how you might define a `traced` decorator:

```lisp
(define-decorator-dispatcher traced (fdef &key options &allow-other-keys)
  (destructuring-match options
    ((&key (to '*trace-output*)
           (when 't))
     (destructuring-match fdef
       ((defun name arglist &body doc/decls/forms)
        (:when (eq defun 'defun))
        (multiple-value-bind (doc decls forms) (parse-docstring-body doc/decls/forms)
          (let ((<when> (make-symbol "WHEN"))
                (<s> (make-symbol "S")))
            `(defun ,name ,arglist
               ,@(if doc (list doc))
               ,@decls
               (let ((,<when> ,when)
                     (,<s> ,to))
                 (when ,<when>
                   (format ,<s> "[~S" ',name))
                 (multiple-value-prog1
                     (progn
                       ,@forms)
                   (when ,<when>
                     (format ,<s> "]~%" ',name))))))))
       (otherwise
        (decorator-error "not a function definition: ~S" fdef))))
    (otherwise
     (decorator-error "unexpected options for traced ~S" options))))
```

And now

```lisp
(defvar *traced* nil)

#@(traced :when *traced*)
(defun foo (x)
  x)
```

is expanded to

```lisp
(defun foo (x)
  (let ((#:when *tracing*) (#:s *trace-output*))
    (when #:when (format #:s "[~S" 'foo))
    (multiple-value-prog1
        (progn x)
      (when #:when (format #:s "]~%" 'foo)))))
```

**`make-decorator-readtable`** constructs a readtable with a dispatch macro, by default for `@`, for decorators.  Note that `@` will clash with `read-package` in my hax.

**`define-decorator-dispatcher`** defines a decorator dispatcher.  Dispatchers take one mandatory argument which is the form they are decorating, and several keyword arguments:

- `pre` is the current list of forms to be evaluated before the decorated form;
- `post` is the current list of forms to be evaluated after the decorated form;
- `options` is any options to the decorator -- in syntax like `#@(traced . options) <form>` it is the options, while in a form like `#@traced <form>` it is the empty list;
- `prefix` is the prefix argument to the dispatch macro.

Decorator dispatchers return one, two or three values:

- one value is the rewritten form;
- two values are the rewritten form and the new list of pre forms;
- three values are the rewritten form, the new list of pre forms, and the new list of post forms.

**`decorator-error`** is both the condition type of errors signalled by `decorators` and a function which will signal such an error taking a format string and format arguments.

There are predefined decorators called `inline` and `notinline`.

As well as decorators defined with `define-decorator-dispatcher` you can simply define functions which will be called.  The reason for `define-decorator-dispatcher` is just so that `inline` &c can work.

### Notes
In general a decorated form ends up expanding to

```lisp
(progn
  <pre-form>
  ...
  <rewritten-form>
  <post-form>
  ...)
```

This is done so things work at the top level (in particular I did not want anything which ended up being `(let ((v (defun ...))) ... v)`, but it means that anything which adds post forms needs to be aware that the value of the form will be lost.

`decorators` will probably stop being a toy soon.

### Package, module
`decorators` lives in and provides `:org.tfeb.toys.decorators`.

---

The TFEB.ORG Lisp toys are copyright 1990-2022 Tim Bradshaw.  See `LICENSE` for the license.

---

[^1]:	However `gather` can now be used as `(gather ... (for (x ...)) ...)` for instance.

[^2]:	Not quite, because it only really knows how to modify a (copy of) a readtable.

[^3]:	I once had a much more elaborate system along these lines based around having looked at, I think, C++'s version of this sort of idea (or was it Java's?  I forget), and this system did force friendship relations to be bidirectional.  I probably still have that code somewhere, and I might one day revive it.

[^4]:	There was a previous `fluids` module.  The name 'fluid' comes from Cambridge Lisp / Standard Lisp, although it might go back further than that.  In those languages you would declare a variable 'fluid' which told the compiler that the full dynamic-binding semantics for it should be kept, even in compiled code.  Of course for both those languages compiled code and interpreted code often had different semantics: I am pretty sure that *all* variables in interpreted code were implicitly fluid.