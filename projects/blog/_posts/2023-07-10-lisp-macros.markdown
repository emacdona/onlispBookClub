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
We've since strayed a bit from that goal, but we keep working our way back towards it. We just can't help ourselves from
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

<!---
So the question becomes: What do we want an example to show? Well, I would propose that the answer to that question is
the same as this one:
What makes macros so powerful? What can I do with a macro that I can't with another language?

Lisp macros give you (the programmer) power traditionally reserved for the language compiler and language interpreter
authors. They do this
by providing you (the programmer) a way to transform syntax during the Lisp evaluation process. When writing code (a
macro) to transform syntax,
you have the full power of the Lisp language at your disposal.

Given that answer, the route I took was this: Find a feature in another language that Lisp doesn't have... and add it to
Lisp!
--->

## The Example

For an example, I've found a language feature that I like in another language that lisp doesnt' have... and will add it
to lisp. To do this, my macro will take as input the syntax that I want, and transform
it to syntax that my Lisp implementation understands. This new syntax will have exactly the same semantics I desire for
my added syntax.

Scala has a special variable, `_`, that takes on a variety of meanings -- each depending on context. In the context of
defining new funcitons based on [partial application](https://en.wikipedia.org/wiki/Partial_application) of existing
functions, it allows you to specify
which function parameters you wish to remain variable in your new function definition.

```scala
def y(m: Float, x: Float, b: Float) = m * x + b

def slopeInterceptLine(slope: Float, intercept: Float) = y(slope, _, intercept)

def y1 = slopeInterceptLine(2, 0)
def y2 = slopeInterceptLine(2, -1)

val indexes: List[Float] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

println(List(indexes.map(y1), indexes.map(y2)))
```

Let me start by saying that I am surely neither a Lisp nor a Lisp Macro expert. However, if you've never heard about
Lisp macros, there is a decent chance I may know more than you about them. Which puts me in the position
many first time macro studiers find themselves in: I want to tell someone who knows less about macros that I do all
about macros!

The funny part about finding yourself in this position is that once there, you **immediately** find yourself in the
position of answering the question:

"What can I do with macros that I can't already do in my programming language of choice?"

You'll hear various answers to this question, such as:
projects/blog-terraform/terraform.tfstate
"Well, Lisp and your favorite programming language are both Turing complete, the answer is: there is nothing your
language can't do that Lisp can do." -- which, while technically correct is pretty unsatisfying. Turing completeness
doesn't measure expressiveness.

or

"Well, you can create expressions that look like function calls but give you complete control over 'when', 'if', and '
how often' their arguments are evaluated." This is a great answer, the problem is it's not very constructive.

Personally, I prefer to learn things by looking at simple, but non-trivial examples.

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
