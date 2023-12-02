---
layout: post
published: false
---

# _Draft:_ (Yet Another) Lisp Macro Blog Post

_Note: as long as "Draft:" is in the title, this post may undergo significant changes_

## The Inspiration

I have this thing where I meet with a group of like-minded nerds once a week and discuss Lisp topics. Ostensibly, the
group started as a book club, and the goal of that club was to read Paul Graham's
book ["On Lisp"](http://www.paulgraham.com/onlisp.html).
We constantly find ourselves drifting from that goal, but we keep working our way back towards it. We just can't help ourselves from
being distracted by various Lisp related topics.

As an aside: lots of great books have been written about Lisp[^1]. To me, at least, it seems that **most** books written
about Lisp are fantastic. Even if you care nothing about the language (you should),
it's worth your time to pick up one of the more well known books about Lisp -- so that you have an example of quality
technical writing to study.

But we were talking about one book in particular: _On Lisp_. _On Lisp_ covers a lot of topics, but if one were to ask
the Lisp community:

"Which **one** Lisp book should I read if I want to learn how to write Lisp Macros?"

I believe the community would answer:

"On Lisp."

So it should surprise no one that when a group of nerds gets together to read **"the"** book about Lisp macros, at least
one of those nerds will feel the need to write a blog post about macros. This is one such post.

## The Problem

I've read a lot of articles and blog posts that try to explain the power of Lisp macros. They talk about transformation
of syntax, evaluation of arguments, [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity), etc.
These topics are all important, of course -- but when I want to learn something, it really helps me to see an example.

But creating examples for the purpose of learning or teaching isn't easy. In my opinion, the perfect learning/teaching
example:

* Exhibits the topic being studied in a non-trivial way
* Has an implementation that's small enough to fit on half a page

It can be very hard to strike a balance between those constraints. If an example is "too simple", it feels "useless".
However, any time we move beyond "simple", we're usually assuming knowledge of some other domain from which we are
pulling
our example.

I hope the example I've chosen has met those two constraints.

## The Example

One of the things that makes Lisp macros so powerful is that they allow you to extend Lisp _in the application code_ you are writing -- using the _same_ language (Lisp). Contrast this with, say, the C language.
_If_ you are working with a C compiler which happens to be written in C, you _can_ extend the language (C) in C. However, you can _only_ do so by modifying the compiler. You can't do it in the code of
the applciation you are writing.

As stated above, the example must "Exhibit the topic being studied" -- macros -- "in a non-trivial way". One way of achieving that would be to
extend the Lisp language using a Lisp macro. 

To that end, I did a little window shopping in other language syntaxes looking for a feature that Lisp lacks. I settled on the syntax in Scala that allows you to
use the special variable `_` when defining new functions via [partial application](https://en.wikipedia.org/wiki/Partial_application) of existing functions to
specify which arguments of the existing function you wish to include as arguments of the new function.

That's a mouthful. Consider the [slope-intercept form of a line](https://en.wikipedia.org/wiki/Linear_equation#Slope%E2%80%93intercept_form):
\\(y = mx+b\\)

We think of lines as a function of one independent variable, ie: \\(y = f(x)\\) -- however the function above appears to have three independent variables!
Never fear: mathematicians just call \\(m\\) and \\(b\\) "parameters" -- which, once chosen, define the single function (one of the _many_ functions that describe lines) defining
whatever line it is we happen to care about.

But, that process of specifying two "parameters" for a function of three variables -- to obtain a new function of one variable is a PERFECT example of partial function application! In Scala, that looks like this:

```scala
def y(m: Float, x: Float, b: Float) = m * x + b

def slopeInterceptLine(slope: Float, intercept: Float) = y(slope, _, intercept)

// function that pairs integral inputs with even integers
def y1 = slopeInterceptLine(2, 0)

// function that pairs integral inputs with odd integers
def y2 = slopeInterceptLine(2, -1)

val indexes: List[Float] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

println(List(indexes.map(y1), "\n", indexes.map(y2)))
```

which yields:
```scala
List(List(2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0),
  , List(1.0, 3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0, 17.0, 19.0))
```

Notice how when defining `slopeInterceptLine`, you can pass its formal parameters -- along with a special `_` identifier -- to what looks
like an invocation of `y`. It is not, of course, an invocation -- this is special Scala syntax that allows you to concisely define
functions via partial function application. Part of that syntax is the `_` identifier. In this context, it allows you to specify
a formal parameter of a function which you wish to remain variable when defining a new function via partial application.

I find that when I first set about writing a Lisp macro, it helps to first determine the signature I want the macro to have and then
determine the code I want it to generate. _I save the actual implementation for last_. So, to that end:

```lisp
(defun y (m x b)
  (+ (* m x) b))

(defun slope-intercept-line (slope intercept)

  ;; mypartial is the macro we will write
  ;; we would like it to generate code something like the following:
  ;; (lambda (x) (Y SLOPE x INTERCEPT))
  
  (mypartial y slope _ intercept))
```



<!---
```lisp
(defmacro mypartial (f &rest args)
  (labels ((process-args (args)
             (if args
                 (let ((first (car args))
                       (parameter (gensym)))
                   (multiple-value-bind
                         (rest-parameters rest-arguments)
                       (process-args (cdr args))
                     (if (eq first '_)
                         (values (cons parameter rest-parameters)
                                 (cons parameter rest-arguments))
                         (values rest-parameters
                                 (cons first rest-arguments)))))
                 args)))
    (multiple-value-bind (new-function-parameters all-function-arguments)
        (process-args args)
      `(lambda (,@new-function-parameters) (,f ,@all-function-arguments)))))

(defun y (m x b)
  (+ (* m x) b))

(defun slope-intercept-line (slope intercept)
  (mypartial y slope _ intercept))

(setf (symbol-function 'y1) (slope-intercept-line 2 0))
(setf (symbol-function 'y2) (slope-intercept-line 2 -1))

(let ((indexes '(1 2 3 4 5 6 7 8)))
  (list
   (mapcar #'y1 indexes)
   (mapcar #'y2 indexes)))
```

Why the do we `setf` the `symbol-function` of the symbol we chose as the name of our function -- instead of just
using `defun`? Well, in short: because we
aren't _defining_ a function -- we aren't specifying its arguments and we aren't providing a set of expressions which
make up the function body. Instead, we
already _have_ a function (the one returned by `slope-intercept-line`) and we just want to give it a name. As it turns
out, Common Lisp is a "Lisp-2". This means
that a symbol can be used to name two different types of things (values and functions) -- and which one it evaluates to
will depend on context (whether it is the first
element of a list currently being evaluated or not).
--->

<!---@formatter:off--->
[^1]: Some available free online:
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
