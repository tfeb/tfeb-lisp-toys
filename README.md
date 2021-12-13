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

---

## An iteration protocol: `for`
This lets you define iterators for objects by defining methods on `iter` which typically returns a function.  `next` (another generic function) should then know how to get the next element from an iterator: for an iterator which is a function it just calls it.  `for` is a macro which will loop over objects, calling `iter` to make the iterator & `next` to get the next values.  There are also `range`s, and finally a `gather` macro which is a bit like Python's list comprehensions (or what I thought they were like in 2004).

As an example the method on `iter` for lists is this:

```lisp
(defmethod iter ((l list) &key)
  (lambda ()
    (if l
        (multiple-value-prog1
            (values (car l) t)
          (setf l (cdr l)))
      (values nil nil))))
```

(In fact it's `#'(lambda ...)` which tells you how old this code is.)  You could then gather the even integers in a list:

```lisp
(gather (* x x)
  for (x '(1 2 t 3))
  when (numberp x)
  when (oddp x))
```

Note that `for` & `when` are not `loop`-style keywords: `gather` turns them into `(when ...)`, so they're just the normal CL macros:

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

## Turning `case` into an explicit jump table: `ncase`
`ncase` is just like `case`but it can turn `case` statements into explicit jump tables if they are large enough.  From the comments in the file:

> This is actually not a really good example because the compiler is likely to be able to do a better job of the decisions involved, and it can also use real jumps rather than function calls which only may turn into jumps.

I can't see a case where `ncase` would be useful today, but it's there.

`ncase` lives in `org.tfeb.toys.ncase` and provides `:org.tfeb.toys.ncase`.

## Global lexical variables: `glex`
CL only has global *special* variables, which are dynamically scoped.  But it's easy to fake global variables which are lexically scoped using symbol macros.  That's what `glex` does.

**`defglex`** is like `defvar`: `(defglex x)` will define `x` as a global lexical but not give it an initial value, while `(defglex x 1)` will give it an initial value as well.  You can provide documentation strings.

**`defglex*`** is like `defparameter`: you can't omit the value and it is set each time.

**`make-glex-readtable`** will construct a readtable in which `#$x` refers to a global lexical variable `x`.  It has three keyword arguments:

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

A *readtable maker* is an object which encapsulates a set of changes to make to a readtable – it's almost but not quite a readtable factory[^1].  A *composable* readtable maker is a readtable maker which can be composed with another readtable maker to either produce a readtable maker which will be compatible with both, or signal an error if that is not possible.

Composing composable readtable makers produces a composable readtable maker which is compatible with all of them.  However, making a readtable from this object may still change it in incompatible ways: that problem isn't something I tried to solve.

Composable readtable makers are almost but not quite simple to implement.  The bit which isn't simple is also the thing that limits them: copying character syntax.  This is fiddly because it can be order-dependent in general and because you might want to do things such as swap the syntaxes of two characters.  So this is all avoided: you can only specify how to copy character syntax in a way which is both loop free and order-independent.  Checking this is mildly fiddly.

To deal with limitations like those that apply to character syntax, there are also 'special orders' which are simply lists of functions which get called.

## Decomposing iteration: `simple-loops`
Like a lot of people I have mixed feelings about `loop`.  For a long time I thought that, well, if I wasn't going to use `loop`, I'd need some other elaborate iteration system, although perhaps one which was more principled and extensible such as Richard C Waters' [Series](https://github.com/tfeb/series "Series")[^2].  And I am sure the CL community has invented other tools while I've not been watching.

But now I think that this is, perhaps, chasing a mirage: it might be better *not* to have some vast all-encompassing iteration tool, but instead a number of smaller, independent, components.  For a long time I have written

```lisp
(collecting
  (dotimes (...)
    ...
    (when ...
      (collect ...)
    ...))
```

in preference to `(loop ... when ... collect ... ...)` and `collecting` of course is more general:

```lisp
(collecting
  (iterate search (...)
    ...))
```

Can collect objects during a completely general recursive search, for instance.

`simple-loops` provides a number of simple loop constructs which can be used with tools like [`collecting`](https://tfeb.github.io/tfeb-lisp-hax/#collecting-lists-forwards-and-accumulating-collecting "collecting"), [`iterate`](https://tfeb.github.io/tfeb-lisp-hax/#applicative-iteration-iterate "iterate") and any other tool, as well as a general named escape construct.

**`doing`** and **`doing*`** are like `do` and `do*` except that bindings are more defaulted:

-  `<var>` means `(<var> nil nil)`;
- `(<var> <init/step>)` means `(<var> <init/step> <init/step>)`;
- `(<var> <init> <step>)` means what it currently does.

In addition if no return value is specified the current values of all the bindings are returned.

**`passing`** and **`failing`** are while and until loops which bind variables with the same defaulting as `doing`: `(passing ((x ...) (y ...)) ...)` will iterate until `(and x y)` is true and then return the values of `x` and `y`.  `failing` will iterate until they are *not* all true and then return their values.

**`do-passing`** and **`do-failing`** are like `passing` and `failing` but the test is after the body, so they always iterate at least once.

There are starred forms of all these macros which bind sequentially, for a total of six macros.

**`looping`**  and **`looping*`**  are looping constructs with implicit stepping: `(looping ((a 1) b) ...)` will bind `a` to `1` and `b` to `nil` and then the values of the last form in the body is used to step the values.  There is no termination clause, but there is an implicit block named `nil` around it all and the body is an implicit `tagbody` so you can leap around in the same way you can with `do` and so on.  You can also use `escaping`.  Declarations at the start of the body are lifted to where they belong.  Initial bindings are in parallel for `looping`, in serial for `looping*`.  Here's a program which is not known to halt for all arguments:

```lisp
(defun collatz (n)
  (looping ((m n) (c 1))
    (when (= m 1)
      (return c))
    (values (if (evenp m)
                (/ m 2)
              (1+ (* m 3)))
            (+ c 1))))
```

**`escaping`** provides a general named escape construct.  `(escaping (<escaper> &rest <defaults>) ...)` binds `<escaper>` as a local function which will immediately return from `escaping`, returning either its arguments as multiple values, or the values of `<defaults>` as multiple values.  The forms in `<defaults>` are not evaluated if they are not used: if they are evaluated they're done in their lexical environment but in the dynamic environment where the escape function is called.

`escaping` is obviously a shim around `(block ... (return-from ...) ...)` and there are the same constraints on scope that blocks have: you can't call the escape function once control has left the form which established it.

The `passing` family of functions *aren't* named `while` because they're not actually `while` loops as the bind variables and also I didn't want to take such a useful and short name: everyone knows what `(while (= x 3) ...)` means.

`passing` and friends expand into `do` and `do*`, not `doing` and `doing*` but thy use the same clause-parser that`doing` and `doing*` do so their clauses work the same way.

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
CL, being a Lisp-2, makes it somewhat verbose to pass functions around as arguments: you end up with a lot of `(funcall x ...)`s in code.  `sb-readtable` makes it so that `[x ...]` is read as `(funcall x ...)`, and it can do other things as well.

The readtable constructed by `make-sb-readtable` lets you read forms like `[...]`.    Normally `[...]` reads as `(<op> ...)` where `<op>` is the value of `*sb-operator-name*`. (which by default is `funcall`).  However, if `*sb-transformer*` is not `nil`, then it is used to transform the form read into anything it wants to.  Note that both of these things happen at read time.

**`make-sb-readtable`** returns a readtable in which `#\[` is defined to read a form as described above, and `#\]` is an error.  It has two optional arguments: `from-readtable` is the readtable to copy, defaultly `*readtable*`, and `to-readtable` is the readtable to copy into, defaultly `nil`: these have the same semantics as for `copy-readtable`.

**`*sb-operator-name*`** is the operator name interposed by default in `[...]` syntax.  Defaultly it is `funcall`.

**`*sb-transformer*`** is either `nil` or a designator for a function of two arguments: the form read by `[...]` and the stream it was read from.  Its result is used as the value of the read form.  It's passed the stream so it can signal useful read-time errors.  Default value is `nil`.

`sb-readtable` lives in `org.tfeb.toys.sb-readtable` and provides `org.tfeb.toys.sb-readtable`.

---

The TFEB.ORG Lisp toys are copyright 1990-2021 Tim Bradshaw.  See `LICENSE` for the license.

---

[^1]:	Not quite, because it only really knows how to modify a (copy of) a readtable.

[^2]:	This link is to my own copy.

[^3]:	I once had a much more elaborate system along these lines based around having looked at, I think, C++'s version of this sort of idea (or was it Java's?  I forget), and this system did force friendship relations to be bidirectional.  I probably still have that code somewhere, and I might one day revive it.

[^4]:	There was a previous `fluids` module.  The name 'fluid' comes from Cambridge Lisp / Standard Lisp, although it might go back further than that.  In those languages you would declare a variable 'fluid' which told the compiler that the full dynamic-binding semantics for it should be kept, even in compiled code.  Of course for both those languages compiled code and interpreted code often had different semantics: I am pretty sure that *all* variables in interpreted code were implicitly fluid.