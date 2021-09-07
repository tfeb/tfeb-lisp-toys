# [TFEB.ORG Lisp toys](https://github.com/tfeb/tfeb-lisp-toys "TFEB.org Lisp toys")
This repo contains a collection of small Common Lisp toys I've written over the last thirty-odd years.  These are explicitly *toys*: they're little things which I wrote for amusement and which I decided might one day be useful either to me or someone else.

These toys may make their way into a more formal place, such as my [Lisp hax repo](https://github.com/tfeb/tfeb-lisp-hax "TFEB.ORG Lisp hax").  When that happens they'll vanish from here (perhaps with a note).  There will therefore never be releases from this repo and you should never assume stability of what's here.  These are just, well, toys.

The documentation is generally rather sparse.  Some of the toys may work, some may not.  Some may never have worked.

## General
### Modules
All of these things are independent modules, providing something which looks like `org.tfeb.toys.<module>`.  There is no ASDF or other system definition for all of them.  If they need other things either from amongst themselves or from other modules I've written they'll do so using [`require-module`](https://github.com/tfeb/tfeb-lisp-tools#requiring-modules-with-searching-require-module "require-module") which you'll need to have installed in that case, or if you don't to have loaded those modules some other way.  In the case that they need things written by other people or other larger systems they'll do that using [Quicklisp](https://www.quicklisp.org/ "Quicklisp").  Some of them do the former, none currently do the latter.

### Portability
All of the toys purport to be portable Common Lisp.

### Zero history
The repo from which the toys are published was invented in 2021, but some of them are much older than that.  Some of that history exists but not in the publication repo.

### Naming conventions
All of the toys use *domain-structured names*: packages, modules, features and so on have names which start with a reversed DNS domain and then continue to divide further.  The prefix for all of the toys is `org.tfeb.toys`.  See [the TFEB.ORG tools documentation](https://github.com/tfeb/tfeb-lisp-tools#naming-conventions "TFEB.ORG tools / Naming conventions") for a little more on this.  If they move elsewhere, these names will change.

----

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

----

The TFEB.ORG Lisp toys are copyright 1990-2021 Tim Bradshaw.  See `LICENSE` for the license.

[^1]:	Not quite, because it only really knows how to modify a (copy of) a readtable.

[^2]:	This link is to my own copy.

[^3]:	I once had a much more elaborate system along these lines based around having looked at, I think, C++'s version of this sort of idea (or was it Java's?  I forget), and this system did force friendship relations to be bidirectional.  I probably still have that code somewhere, and I might one day revive it.