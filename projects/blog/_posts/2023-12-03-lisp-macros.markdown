---
layout: post
published: true
---

# Draft:[^draft] (Yet Another) Lisp Macro Blog Post

## The inspiration for this post

Once a week, I meet with a group of like-minded nerds to discuss Lisp topics. This group started as a book club, and our
original goal was to read Paul Graham's book "[On Lisp](http://www.paulgraham.com/onlisp.html)". Although we constantly
find
ourselves drifting from that goal, we keep working our way back toward it. We just can't help ourselves from being
distracted by various Lisp related topics.

{:.callout}
Lots of great books have been written about Lisp[^books]. To me at least, it seems that **most** books
written about Lisp are fantastic. Even if you care nothing about the language (you should), it's worth your time to pick
up one of the more well known books about Lisp -- so that you have an example of quality technical writing to study.

_On Lisp_ covers a lot of topics, but if one were to ask the Lisp community:

"Which **one** Lisp book should I read if I want to learn how to write Lisp Macros?"

I believe the community would answer:

"On Lisp."

So it should surprise no one that when a group of nerds gets together to read **"the"** book about Lisp macros, at least
one of those nerds will feel the need to write a blog post about macros. This is one such post.

## Searching for an example

I've read a lot of articles and blog posts that try to explain the power of Lisp macros. They talk about transformation
of syntax, evaluation of arguments, [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity), etc. These topics are
all important, of course -- but when I want to learn something, it really helps me to see an example.

Coming up with a good example isn't easy, however. In my opinion, a good example should:

* Exhibit the topic being studied in a non-trivial way.
* Have an implementation that's small enough to fit on half a page.

I'll admit that I don't have a rigorous definition for "non-trivial". The best I can do is give some examples of what
I don't like about examples I consider to be "trivial":

* On their own, they don't convince me that the topic under study has any useful applications.
* They show me something that already seemed obvious.

What follows is my attempt at a "non-trivial" example whose implementation fits on half a page.

## A non-trivial example

One of the things that makes Lisp macros so powerful is that they grant application developers the ability to extend the
language
(Lisp). Contrast this with, say, C. Only compiler developers (not application developers) have the ability to extend C.

This suggests a possible avenue to explore when searching for a non-trivial example: find a feature in another language
and add it to Lisp!

To that end, I went shopping for language features that Lisp lacks. I settled on the syntax in Scala that allows you to
use the identifier '`_`' when defining new functions
via [partial application](https://en.wikipedia.org/wiki/Partial_application). This syntax allows you to specify which
arguments of a given function you wish to remain as formal parameters of the new function you are defining. We'll get
to an example in Scala in a minute, but first...

### Slope-intercept form of a line

Consider
the [slope-intercept form of a line](https://en.wikipedia.org/wiki/Linear_equation#Slope%E2%80%93intercept_form):
\\(y = mx+b\\)

We think of lines as a function of one independent variable, ie: \\(y = f(x)\\) -- however the function above appears to
have three independent variables: \\(m, x, b\\)!

So, what gives? Well, mathematicians just call \\(m\\) and \\(b\\) "parameters" [^param] [^paramdef] -- which, once chosen, determine the single
function (one of _many_) which defines whatever line it is we happen to care about.

We're programmers, not mathematicians, however -- and to a programmer, that answer may seem unsatisfying...

### Definining functions using partial application in Scala

This mathematicians' process of specifying two "parameters" for a function of three variables -- to obtain a new function of one
variable -- is a PERFECT example of partial function application! In Scala, that looks like this:

```scala
def y(m: Float, x: Float, b: Float) = m * x + b

// Here, we are creating a function, 'slopeInterceptLine', that in turn 
// allows us to create new functions of a single variable by:
// 1) Fixing two of the parameters ('slope' and 'intercept') of the 
//    function 'y'.
// 2) Making the remaining parameter, 'x', a parameter of the new 
//    function being created.
// Note how we use '_' to specify the parameter(s) we wish to be 
// parameter(s) in the new function being created.
def slopeInterceptLine(slope: Float, intercept: Float) = y(slope, _, intercept)

// Use 'slopeInterceptLine' to create a new function of a single variable.
// In this case, a function that pairs integral inputs with even integers
def y1 = slopeInterceptLine(2, 0)

// Use 'slopeInterceptLine' to create a new function of a single variable.
// In this case, a function that pairs integral inputs with odd integers
def y2 = slopeInterceptLine(2, -1)
```

Notice how when defining 'slopeInterceptLine', you can pass its formal parameters -- along with a special '`_`'
identifier -- to what looks like an invocation of 'y'. It is not, of course, an invocation; it's special Scala
syntax that allows you to concisely define functions via partial function application. '`_`' is part of that special
syntax. 

Once defined, we can then call our newly defined functions on some values:

```scala
val indexes: List[Float] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

println(List(indexes.map(y1), "\n", indexes.map(y2)))
```

Which yeilds:

```scala
List(List(2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0),
  , List(1.0, 3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0, 19.0))
```

### Adding this feature to Lisp using a macro

Now that we've found a feature we'd like to add to Lisp, let's get to it.
I find that when I first set about writing a Lisp macro, it helps to first determine the signature I want the macro to
have and then determine the code I want it to generate. _I save the actual implementation for last_. So, to that end:

```lisp
(defun y (m x b)
  (+ (* m x) b))

(defun slope-intercept-line (slope intercept)

  ;; partial is the macro we will write
  ;; we would like it to generate code something like the following:
  ;; (lambda (x) (y slope x intercept))
  
  (partial y slope _ intercept))
```

So, let's do this in steps. First, we know that our macro will take as arguments:

1. A function whose partial application we wish to use to build a new function
2. An optional list of arguments. In this list, we expect to be able to use '`_`' for parameters we wish not to fix in the
   partial application.

As shown in the code above, we'd like it to return a lambda whose formal parameters correspond to those in the arg list
specified as '`_`'. We would like that lambda to "fix" the other arguments by creating a lexical closure over them.

Now, wouldn't it be nice if we had a variable called 'new-function-arguments' that was a list of all the formal
parameters to our new function, and a variable called 'all-function-arguments' that was a list of all arguments used to
invoke the function 'f', which we are partially applying? For now, let's assume they exist!

```lisp
(defmacro partial (f &rest args)
;;
;; missing code here
;;
      `(lambda (,@new-function-parameters) (,f ,@all-function-arguments)))
```

Okay... so, how do we get the values of those variables? Well... let's assume we have a function called 'process-args'
that can, given the list of arguments passed to the macro, retrieve them for us. Note that _this_ is the function that
will define the semantics of the '`_`' identifier! We've cleverly separated it out from the rest of the macro.

```lisp
(defmacro partial (f &rest args)
;;
;; missing code here
;;
    (multiple-value-bind (new-function-parameters all-function-arguments)
        (process-args args)
      `(lambda (,@new-function-parameters) (,f ,@all-function-arguments))))
```

So, what does this function need to do? Well, it needs to iterate over the 'args' passed to the macro. For each symbol
in 'args':

1. If it _is not_ '`_`', it adds it to the 'all-function-arguments' list that it's building.
2. If it _is_ '`_`', it replaces it with a new symbol, and adds that symbol to both the 'new-function-parameters' and
   the 'all-function-arguments' lists that it's building.

The symbols added to 'all-function-arguments' in step one are those we are holding fixed. The lambda that our macro creates
passes them in to the function we are partially applying. These symbols are expected to have meaning in the context in
which this macro was expanded. The lambda that the macro results in will capture the bindings for these symbols, and --
if returned as the result of a function call -- create a lexical closure for these bindings. Anywhere the lambda is
used, these symbols will evaluate to the same values captured in the closure.

The symbols created in step two are those needed for the formal parameters when we define the lambda -- so they are
added to the 'new-function-parameters' list. They are _also_ added to the 'all-function-arguments' list -- because they must
be passed to the function we are partially applying.

Let's have a look.

```lisp
(defmacro partial (f &rest args)
  (labels ((process-args (args)
             (if args
                 ;; if args isn't empty
                       ;; Store the first element of 'args' in a variable
                 (let ((first (car args))
                       ;; Generate a new symbol, and store it in a variable.
                       ;; We'll use this later, *if* 'first' is '_'
                       (parameter (gensym)))
                   (multiple-value-bind
                         (rest-parameters rest-arguments)
                       ;; Recur over the remaining args. Note that we expect 
                       ;; this function to return two values
                       (process-args (cdr args))
                     (if (eq first '_)
                         ;; If the first arg is '_', then cons the new symbol 
                         ;; we created onto the front of both lists
                         (values (cons parameter rest-parameters)
                                 (cons parameter rest-arguments))
                         ;; If the first arg is not '_', then cons it only onto 
                         ;; the list of arguments our lambda will pass to the 
                         ;; function we are partially applying. Do not add it 
                         ;; to the list of formal parameters for the lambda 
                         ;; we are creating.
                         (values rest-parameters
                                 (cons first rest-arguments)))))
                 ;; else, just return empty args
                 args)))
    (multiple-value-bind (new-function-parameters all-function-arguments)
        (process-args args)
      `(lambda (,@new-function-parameters) (,f ,@all-function-arguments)))))
```

We can test our macro:

```lisp
(macroexpand-1 '(partial slope-intercept-line 2 _ 0))
```

Which yields:

```lisp
(LAMBDA (#:G595) (SLOPE-INTERCEPT-LINE 2 #:G595 0))
```

Once that macro is defined, we can write the following code[^setf]:

```lisp
(defun y (m x b)
  (+ (* m x) b))

(defun slope-intercept-line (slope intercept)
  (partial y slope _ intercept))

(setf (symbol-function 'y1) (slope-intercept-line 2 0))
(setf (symbol-function 'y2) (slope-intercept-line 2 -1))

(let ((indexes '(1 2 3 4 5 6 7 8 9 10)))
  (list
   (mapcar #'y1 indexes)
   (mapcar #'y2 indexes)))
```

Which yields:

```lisp
((2 4 6 8 10 12 14 16 18 20) (1 3 5 7 9 11 13 15 17 19))
```

The same as the Scala code. We've added a feature to Lisp! Is this feature worthwhile? That's debatable. For one, the
macro call is barely shorter than lambda expression. But "worthwhile" wasn't the intent. I contend that this example is,
at least, non-trivial ;-)

<!---@formatter:off--->
[^draft]: As long as "Draft:" is in the title, this post may undergo significant changes. I've published it so that I 
    could solicit feedback from colleagues.

[^param]: Not to be confused with the "formal parameters" of a function definition in a programming language -- which I
    also happily refer to as "parameters" in this very same blog post. I count on context (and the reader's keen intellect)
    to distinguish between the two uses. These are the dangers of mixing domains (here: math and programming) in the same
    article.

[^paramdef]: Mathematicans would possibly notate this as such: \\(y = f_{m,b}(x)\\)

[^setf]: Why the do we `setf` the `symbol-function` of the symbol we chose as the name of our function -- instead of
    just using `defun`? Well, in short: because we aren't _defining_ a function -- we aren't specifying its arguments and we
    aren't providing a set of expressions which make up the function body. Instead, we already _have_ a function (the one
    returned by `slope-intercept-line`) and we just want to give it a name.

[^books]: Some available free online:
    * [Practical Common Lisp](https://gigamonkeys.com/book/)
    * [Common Lisp: An Interactive Approach](https://cse.buffalo.edu/~shapiro/Commonlisp/)
    * [Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/)
    * [On Lisp](http://www.paulgraham.com/onlisp.html)
    * [Paradigms of Artificial Intelligence Programming](https://github.com/norvig/paip-lisp)

    Others available for purchase:
    * [Common Lisp Recipes](http://weitz.de/cl-recipes/)
    * [ANSI Common Lisp](http://www.paulgraham.com/acl.html)
    * [Lisp in Small Pieces](https://www.cambridge.org/core/books/lisp-in-small-pieces/66FD2BE3EDDDC68CA87D652C82CF849E)
<!---@formatter:on--->
