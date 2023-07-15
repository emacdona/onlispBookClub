---
layout: post
---

# Pondering Lisp Macros

## The Impetus

I have this thing where I meet with a group of like-minded nerds once a week and discuss Lisp topics. Ostensibly, the
group started as a book club, and the goal of that club was to read Paul Graham's book ["On Lisp"](http://www.paulgraham.com/onlisp.html).
We've since strayed a little bit from that goal, but we keep working our way back towards it (we just keep getting distracted by various Lisp related topics).

Lots of great books have been written about Lisp. In fact -- and this is based purely on my gut feeling -- it seems **most** books written about Lisp are
fantastic. Even if you care nothing about the language (you should), it's worth your time to pick up one of the more well known books about Lisp -- so that you have
an example of quality technical writing to study.

But we were talking about one book in particular: On Lisp. On Lisp covers a lot of topics, but if one were to ask the Lisp community:

"Which **one** Lisp book should I read if I want to learn how to write Lisp Macros?"

I believe the community would answer: 

"On Lisp." 

So it should surprise no one that when a group of nerds get together to read **"the"** book about Lisp macros, at least one of those nerds will feel the need to write a blog post about macros. This is such a post.

## Lisp and Lisp Macro Crash Course

I'm assuming your coming to this with some background on Lisp because for me to summarize the entire language here wouldn't do it justice. Meh, I'll try it anyway.

### Everything is either an expression or a value

Lisp doesn't have statements, just expressions. These expressions start with '(' and end with ')'. They can have any number of expressions nested within them. Every expression can be evaluated to a value.


Atoms are 

## An Example

Let me start by saying that I am surely neither a Lisp nor a Lisp Macro expert. However, if you've never heard about Lisp macros, there is a decent chance I may know more than you about them. Which puts me in the position
many first time macro studiers find themselves in: I want to tell someone who knows less about macros that I do all about macros!

The funny part about finding yourself in this position is that once there, you **immediately** find yourself in the position of answering the question:

"What can I do with macros that I can't already do in my programming language of choice?"

You'll hear various answers to this question, such as:

"Well, Lisp and your favorite programming language are both Turing complete, the answer is: there is nothing your language can't do that Lisp can do." -- which, while technically correct is pretty unsatisfying. Turing completeness doesn't measure expressiveness.

or

"Well, you can create expressions that look like function calls but give you complete control over 'when', 'if', and 'how often' their arguments are evaluated." This is a great answer, the problem is it's not very constructive.

Personally, I prefer to learn things by looking at simple, but non-trivial examples. 

```lisp
(defun hello-world ()
    (format nil "Hello World"))
```