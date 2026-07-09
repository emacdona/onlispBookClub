---
layout: post
published: true
title: "DRAFT: Learning NixOS"
---

# DRAFT: Learning NixOS and its Nix Package Manager

{:.callout}
This post is currently in "Draft" status. That means it's subject to change without me noting said changes.

I've been daily driving NixOS for over half a year now. I'm still just a beginner, but I'm slowly learning enough to be dangerous. The more I use it, the more I'm convinced that it's the "correct" way to run software.

It reminds me a lot of when I learned Git. At first, Git was just a collection of arcane commands posing as an alternative to Subversion. But the more I used it, the more I became convinced that it was the "correct" way to version control source files.

My "aha" moment for Git was when I realized how cheap branches and tags were, and how they allowed me to easily create checkpoints anytime I wanted. That meant that if I wanted to hack together an experimental feature just to see if it was possible -- I could create a checkpoint and then try it! If it didn't work, I could throw away the changes (or even check them into a branch if I wanted to try again later) and go back to where I started. Almost instantly. In other words: _Git made running big experiments cheap and risk free_.

My "aha" moment for NixOS was when I realized that I could put any piece of software on my system, including its _entire_ dependency graph, without affecting any other piece of software that's already on my system. At the moment, I can confidently say that _NixOS makes running big experiments risk free_, but I still have a bit to learn before I can add the "_cheap_" qualifier. I have no doubt that NixOS is fully capable of making big experiments "_cheap_"; my current inability to make it do so is due only to my lack of knowledge.

I'm still climbing the NixOS learning curve. To that end, I did a deep dive into NixOS this weekend, trying to get an idea of how it does what it does. My learnings follow.

## Documentation

I've read some things on the [NixOS Wiki](https://wiki.nixos.org/wiki/NixOS_Wiki) and I've read quite a bit of the [Nix Pills](https://nixos.org/guides/nix-pills/00-preface.html) (and followed along by running their examples).

Both are fine resources... but they always left me wanting. It always seemed that they were too precise about things I didn't care about and too imprecise about things I did care about.

Regardless, spend some time on the pills. I did -- so any advice I give should be prefixed with "try doing the pills first".

That being said, read the first three chapters of Eelco Dolstra's [PhD thesis](https://edolstra.github.io/pubs/phd-thesis.pdf). Trust me.

## What the Nix Package Manager Can Do

The Nix Package Manager is extraordinarily good at managing dependencies for software. When you install a Package on NixOS, you can be certain that the components that it depends on will never change as long as it remains installed on your system.

The Nix Package Manager will allow you to upgrade any component to a new version while leaving the current version untouched. If you don't like the new version, you can almost instantly rollback.

The Nix Package Manager is also extraordinarily good at letting any component depend on any version of any other component. And by version, I don't mean in the sense of '1.0.1'; I mean something far more granular. You can have one program use version '1.0.1' of a library that was compiled with optimizations turned on... and another program depend on version '1.0.1' of the same library compiled with optimizations turned off! Both '1.0.1' versions of the same library can exist on your machine, and either can be used to build any program that depends on them. Because building software is an interesting use case for me, this is the feature that I wanted to explore.

## C Libs and Executables

If you've been using Linux for a while, there's a good chance that you've built some program and/or library from C source code. Over the decades, the process of doing so has become remarkably similar across projects. Tools like [Autoconf](https://www.gnu.org/software/autoconf/), [Automake](https://www.gnu.org/software/automake/), and [Libtool](https://www.gnu.org/software/libtool/) have helped make this possible.

If you've ever opened up a Makefile that ships with the C source for a program you've built, you probably noticed that you can change the build process by specifying environment variables.

`LDFLAGS`{:.language-shell .highlight} and `CFLAGS`{:.language-shell .highlight} allow you to control compiler and linker flags. You can use these to specify build time options (where to look for header files; where to look for libraries). You can also use them for things like adding debug symbols or choosing the compiler optimization level -- in which case, given the exact same source, the values passed to these environment variables can result in significantly different binaries.

`DESTDIR`{:.language-shell .highlight} and `PREFIX`{:.language-shell .highlight} are two environment variables that let you determine where a given piece of software is installed.

Using these four variables, you can control where your project looks for its build time dependencies and where it installs to. This is one way that the Nix Package Manager can control where software is installed and where it finds its dependencies.

## My Experiment

After months of using NixOS as my primary Linux distro, reading about NixOS, and asking Claude lots of questions... I had finally arrived at what I thought was a useful experiment that would let me learn a little about how NixOS worked.

* I wanted to create a simple library whose build process could be influenced by an environment variable in a way that was easily witnessed by an end user.
* I wanted to create a simple program that linked against this library.
* I wanted the build processes of both of these projects to follow familiar conventions.
* I wanted to show that the Nix Package Manager could build both of these projects with no changes to their source or build files.
* I wanted to show that the Nix Package Manager could capture the dependency of the program on the library... and furthermore that it would allow the program to specify bespoke configurations of the library build to depend upon.
* Finally, I wanted to show that multiple versions of program and library could exist on the system without conflict.

## The Code

I'll include some code in this post (I tried to keep the examples small), but all of the code can also be found [here](https://github.com/emacdona/nixdemo). With the caveat, of course, that the code in the repo may evolve.

### The Library

As mentioned above, I wanted to have a library whose behavior could be changed at build time (via an environment variable) in such a way that was easily witnessed by a user. In other words, I won't be modifying `LDFLAGS`{:.language-shell .highlight} or `CFLAGS`{:.language-shell .highlight}[^technicallymodifying] -- because such changes are not easily witnessed.

Instead, my library will use a preprocessor macro to determine the string its single method returns:

```c
#include "greeting.h"

/* GREETING_MESSAGE is defined at compile time via -D flag */
#ifndef GREETING_MESSAGE
#define GREETING_MESSAGE "Hello, World!"
#endif

const char* get_greeting(void) {
    return GREETING_MESSAGE;
}
```

The Makefile that builds my library will allow the value of this macro to be overridden via an environment variable:

```make
# ... code removed, see repo for entire file ...

GREETING ?= Hello, World!

CFLAGS += -DGREETING_MESSAGE='"$(GREETING)"'

# ... code removed, see repo for entire file ...

%.o: %.c greeting.h
	$(CC) $(CFLAGS) -c $< -o $@

# ... code removed, see repo for entire file ...
```

### The Program

The program is even simpler. It just calls the method provided by the library:

```c
#include <stdio.h>
#include <greeting.h>

int main(void) {
    printf("%s\n", get_greeting());
    return 0;
}
```

Likewise, the Makefile that builds the program is simpler than the one that builds the library. In fact, there's nothing really worth pointing out within it. Feel free to look at it in the source repo.

### Building and Installing without the Nix Package Manager

The Makefiles for building the library and the program follow the conventions mentioned earlier. In particular, they install into a directory determined by the `DESTDIR`{:.language-shell .highlight} and `PREFIX`{:.language-shell .highlight} environment variables.

I don't recommend it, but if you wanted, you could `make && make install`{:.language-shell .highlight} the library and then the program... and it should work as expected. The reason I don't recommend this is because it would install both in the `/usr/local`{:.language-shell .highlight} directory tree. This is a blog post about NixOS... we want to have things installed in the Nix Store.

## NixOS Terms and Definitions[^usebeforedefining]

Now that we have source code for a program and a library that it depends on, we can proceed to see how we can use the Nix Package Manager to build and install it. But before we do that, we'll need to define some terms.

These definitions reflect my current mental model of how NixOS works. I won't claim they are "correct". One of my problems with the NixOS ecosystem is that it's sometimes very hard to pin down what a given word means in a given context. These definitions are my attempt to at least capture my understanding.

### "Nix"

The word "Nix" itself can mean one of three different things. I've tried to take care not to use just "Nix" in this document. Instead, I try to explicitly use one of the following three terms.

#### Nix Expression Language

The Nix Expression Language is the programming language in which Nix Expressions are written.

#### Nix Package Manager

The Nix Package Manager is the suite of tools that build Derivations and manage the Nix Store.

The Nix Package Manager can be used to maintain a Nix Store on distros other than NixOS.

#### NixOS

A Linux distro whose entire configuration (not just the applications you install within it) is in the Nix Store.

### Expression
Independent of the Nix ***Expression*** Language, an ***Expression*** is a syntactical object (of a programming language) that evaluates to a value.

The Nix ***Expression*** Language is functional: "Everything is an ***Expression***". This model should be familiar to those who have used functional languages.

You can compose ***Expressions*** into larger ***Expressions***.

### Derivation
Some Expressions evaluate to ***Derivations***. Though there are no such formal terms, in the Nix Expression Language it may be helpful to think of such Expressions as "top level Expressions" or "programs".

The ***Derivation*** that these Expressions evaluate to is an in-memory structure.

When one of these "in-memory ***Derivations***" is Instantiated, a store object is created. This object is ALSO called a ***Derivation***. You can think of this "store ***Derivation***" as a build plan for a set of Outputs. It can be Realized, resulting in the creation of these Outputs.

The key insight here is:

The store ***Derivation*** is completely determined by the inputs to the Expression whose evaluation yielded the in-memory ***Derivation*** (whose Instantiation resulted in the store ***Derivation's*** creation). For the store ***Derivation***, all "variability" has been removed. It is a fully specified "build plan" for a set of Outputs.

### Outputs

A Derivation can be Realized to create multiple ***Outputs***. For example, a Derivation could have separate ***Outputs*** for its runtime and its documentation. All such ***Outputs*** (once Realized) live in the Nix Store.

### Nix Store

The Nix Store is the Nix Package Manager's database. It's implemented right on the filesystem and is usually located at `/nix/store`{:.language-shell .highlight}.

It's a flat database: each directory or file in the `/nix/store/`{:.language-shell .highlight} directory is a single addressable component in the store -- and these directories and files are the only components of the store. Most of them are Derivations and Outputs.

### Store Path

A ***Store Path*** is the coordinate (in the Nix Store) of a given store component (Output, Derivation).

### Package

A ***Package*** is really nothing more than a name given to an Expression that evaluates to a Derivation.

It helps to consider an example:

Imagine a function named `vim`{:.language-shell .highlight} that takes an `enableGui`{:.language-shell .highlight} argument (whose default value is `false`{:.language-shell .highlight}) and returns a Derivation. Imagine that your NixOS distro, in some global namespace, assigned a name to this Expression: `vim-no-gui = vim {}`{:.language-shell .highlight}

That name (`vim-no-gui`{:.language-shell .highlight}) is a ***Package***.

If you wanted to create a Derivation whose Realization would result in a version of `vim`{:.language-shell .highlight} that **did** have a GUI, you could just call that function with `enableGui=true`{:.language-shell .highlight}, eg: `vim {enableGui = true}`{:.language-shell .highlight}.

But NixOS maintainers are also free to create a ***Package*** that does the same by simply assigning a name: `vim-gui = vim {enableGui = true}`{:.language-shell .highlight}.

### Term Summary / Relationships

Putting it all together...

Some Expressions, when evaluated, yield in-memory Derivations. These in-memory Derivations can be Instantiated to become store Derivations. These store Derivations can be Realized as Outputs in the Nix Store.

```
Expression ->
  Evaluate(Expression) ->
    in-memory Derivation ->
      Instantiate(in-memory Derivation) ->
        store Derivation ->
          Realize(store Derivation) ->
            Outputs
```

## Building and Installing with the Nix Package Manager

### Derivation Creating Expressions
To build the library and program with the Nix Package Manager, we create a `default.nix`{:.language-shell .highlight} file in each project's root. This file contains an Expression that defines a function that returns a Derivation[^derivationreturningfunction]. That Derivation contains all the information the Nix Package Manager needs to build the project.

Here is the `default.nix`{:.language-shell .highlight} that builds the `greeter`{:.language-shell .highlight} program:
```nix
{ stdenv
, greeting ? "Hello, World!"
, libgreeting ? import ../lib { inherit stdenv greeting; }
}:

stdenv.mkDerivation {
  pname = "greeter";
  version = "1.0.0";

  src = ./.;

  buildInputs = [ libgreeting ];

  dontConfigure = true;

  buildPhase = ''
    make INCLUDES="-I${libgreeting}/include" LDFLAGS="-L${libgreeting}/lib"
  '';

  installPhase = ''
    make install PREFIX= DESTDIR=$out
  '';

  meta = {
    description = "A greeting program (greeting: ${greeting})";
  };
}
```

Note that the Derivation returned by this function includes `libgreeting`{:.language-shell .highlight} as the only member of its `buildInputs`{:.language-shell .highlight}.

Note that `libgreeting`{:.language-shell .highlight} is passed as a parameter to the function, and its default value is the result of calling the function defined in the lib's `default.nix`{:.language-shell .highlight} file with the same `greeting`{:.language-shell .highlight} parameter passed to this function.

Although the type of `libgreeting`{:.language-shell .highlight} is a Derivation, when it's used to construct the `buildPhase`{:.language-shell .highlight} string, the Nix Expression Language's string interpolation turns it into `libgreeting`{:.language-shell .highlight}'s default Output path in the Nix Store.

The Derivation returned by this function, from the Nix Package Manager's point of view, is completely determined by its inputs[^inputbased]. The key variable input is `libgreeting`{:.language-shell .highlight}, and that input is a Derivation that results from calling the function defined in `lib/default.nix`{:.language-shell .highlight} with the `greeting`{:.language-shell .highlight} parameter.

Here's the `default.nix`{:.language-shell .highlight} that builds the `libgreeting`{:.language-shell .highlight} library:

```nix
{ stdenv
, greeting ? "Hello, World!"
}:

stdenv.mkDerivation {
  pname = "libgreeting";
  version = "1.0.0";

  src = ./.;

  # Pass the greeting to the Makefile via environment variable
  GREETING = greeting;

  # No configure phase needed for this simple Makefile
  dontConfigure = true;

  buildPhase = ''
    make
  '';

  installPhase = ''
    make install PREFIX= DESTDIR=$out
  '';

  meta = {
    description = "A simple greeting library (greeting: ${greeting})";
  };
}
```

Because the Derivation returned by this function uses the value of `greeting`{:.language-shell .highlight} in its definition, that means that this function creates _different_ Derivations for different values of the `greeting`{:.language-shell .highlight} parameter passed to it. The Derivation returned by the function defined in `program/default.nix`{:.language-shell .highlight} can depend on any one of these Derivations.

Since a Derivation is also determined by its inputs, the function defined in `program/default.nix`{:.language-shell .highlight} returns a different Derivation for every different value of `libgreeting`{:.language-shell .highlight}.

In other words, when building `greeter`{:.language-shell .highlight}, each value you choose for `greeting`{:.language-shell .highlight} yields a different Derivation of `libgreeting`{:.language-shell .highlight} to be built. `libgreeting`{:.language-shell .highlight} being the key variable input to `greeter`{:.language-shell .highlight}, this causes a different Derivation of `greeter`{:.language-shell .highlight} to be built. For each value of `greeting`{:.language-shell .highlight}, you get two Outputs in the store: one for `greeter`{:.language-shell .highlight}, one for `libgreeting`{:.language-shell .highlight}.

It's easier to see with an example.

## Running the Build

In the source directory, there are two files, `demo01.nix`{:.language-shell .highlight} and `demo02.nix`{:.language-shell .highlight}. They differ _only_ in the `greeting`{:.language-shell .highlight} they pass to the function defined in `./program/default.nix`{:.language-shell .highlight}, so I'll just show `demo01.nix`{:.language-shell .highlight}:

```nix
let
  pkgs = import <nixpkgs> {};
in
pkgs.callPackage ./program {
  greeting = "Hello from Demo 01!";
}
```

Given those two files, we can run the following two commands:

```bash
nix-build demo01.nix -o demo01
nix-build demo02.nix -o demo02
```

Those commands will (for the single Expression in each file) go through the whole process starting with Expression evaluation all the way through to Output creation.

The first time you run them, you'll see the whole build process (for library _and_ program!) followed by the Nix Package Manager telling you where it placed the program (`greeter`{:.language-shell .highlight}) in the store. If you run them again, the Nix Package Manager recognizes that it has already built them... and just shows you where it put them in the store:

```
$> nix-build demo01.nix -o demo01
/nix/store/32w8g2sjafhj8iag8v2hb1q1s0mmjw56-greeter-1.0.0

$> nix-build demo02.nix -o demo02
/nix/store/hnznyz5p9ymaxx61phsis0q9kh7i1aaw-greeter-1.0.0
```

### Examining the Output

Because we specified the `-o`{:.language-shell .highlight} switch to `nix-build`{:.language-shell .highlight}, it also created two symlinks[^gcroots] in the local directory:

```bash
$> ls -l demo01 demo02
lrwxrwxrwx 1 1024 users 57 Jul  4 19:11 demo01 -> /nix/store/32w8g2sjafhj8iag8v2hb1q1s0mmjw56-greeter-1.0.0
lrwxrwxrwx 1 1024 users 57 Jul  4 19:11 demo02 -> /nix/store/hnznyz5p9ymaxx61phsis0q9kh7i1aaw-greeter-1.0.0
```

We can use these symlinks to run the programs:

```bash
$> ./demo01/bin/greeter
Hello from Demo 01!
$> ./demo02/bin/greeter
Hello from Demo 02!
```

But what about the libraries? Well, we can ask the Nix Package Manager to show us the dependency graph for both executables:

```bash
$> nix-store -q --tree ./demo01 | cat
/nix/store/32w8g2sjafhj8iag8v2hb1q1s0mmjw56-greeter-1.0.0
├───/nix/store/vr7ds8vwbl2fz7pr221d5y0f8n9a5wda-glibc-2.40-218
│   ├───/nix/store/2a3izq4hffdd9r9gb2w6q2ibdc86kss6-xgcc-14.3.0-libgcc
│   ├───/nix/store/hxcmad417fd8ql9ylx96xpak7da06yiv-libidn2-2.3.8
│   │   ├───/nix/store/3rkccxj7vi0p2a0d48c4a4z2vv2cni88-libunistring-1.4.1
│   │   │   └───/nix/store/3rkccxj7vi0p2a0d48c4a4z2vv2cni88-libunistring-1.4.1 [...]
│   │   └───/nix/store/hxcmad417fd8ql9ylx96xpak7da06yiv-libidn2-2.3.8 [...]
│   └───/nix/store/vr7ds8vwbl2fz7pr221d5y0f8n9a5wda-glibc-2.40-218 [...]
└───/nix/store/fhscgmiy6gsjmghbx4nyb9djn68fxjvg-libgreeting-1.0.0
    └───/nix/store/vr7ds8vwbl2fz7pr221d5y0f8n9a5wda-glibc-2.40-218 [...]

$> nix-store -q --tree ./demo02 | cat
/nix/store/hnznyz5p9ymaxx61phsis0q9kh7i1aaw-greeter-1.0.0
├───/nix/store/vr7ds8vwbl2fz7pr221d5y0f8n9a5wda-glibc-2.40-218
│   ├───/nix/store/2a3izq4hffdd9r9gb2w6q2ibdc86kss6-xgcc-14.3.0-libgcc
│   ├───/nix/store/hxcmad417fd8ql9ylx96xpak7da06yiv-libidn2-2.3.8
│   │   ├───/nix/store/3rkccxj7vi0p2a0d48c4a4z2vv2cni88-libunistring-1.4.1
│   │   │   └───/nix/store/3rkccxj7vi0p2a0d48c4a4z2vv2cni88-libunistring-1.4.1 [...]
│   │   └───/nix/store/hxcmad417fd8ql9ylx96xpak7da06yiv-libidn2-2.3.8 [...]
│   └───/nix/store/vr7ds8vwbl2fz7pr221d5y0f8n9a5wda-glibc-2.40-218 [...]
└───/nix/store/sazax1y1k7ab5h5k5m2hbrky7s9dnadb-libgreeting-1.0.0
    └───/nix/store/vr7ds8vwbl2fz7pr221d5y0f8n9a5wda-glibc-2.40-218 [...]
```

If you look closely, you'll see they depend on two different `libgreeting`{:.language-shell .highlight} libraries. Perhaps even more surprising, however, is that they _share_ the same outputs for all other dependencies they have in common. The Nix Package Manager manages all that for you!

## How the Nix Package Manager Enables This

Let's take another look at this diagram:

```
Expression ->
  Evaluate(Expression) ->
    in-memory Derivation ->
      Instantiate(in-memory Derivation) ->
        store Derivation ->
          Realize(store Derivation) ->
            Outputs
```

Now let's justify each step:

### Expression

The Expressions we created (in the `default.nix`{:.language-shell .highlight} files) define functions with parameters of our choosing. Each of these functions builds and returns a Derivation.

We are free to use the arguments passed into this function when it is called to construct the Derivation however we see fit. Perhaps most interesting: we can use these arguments to compute the `inputs`{:.language-shell .highlight} of the Derivation.

For example, given a source code repository (which includes source code along with any scripts and metadata required to convert the source code to an executable artifact), we can create an Expression (function) whose parameters capture any and all variability in our build process. For example: branch name; version number; compiler flags; upstream dependencies; etc.

In the function body, we can specify how a Derivation (and even its inputs) are constructed given the values (arguments) we assign to these parameters.

### Evaluation of Expression

Evaluation of the Expression simply takes the arguments we've passed to the function and uses them to compute the in-memory Derivation.

### In-Memory Derivation

Assuming we're using input addressing[^inputbased], at this point the Store Location of every Output in the entire dependency graph of the Derivation is known (or can be determined).

The store location of _this Derivation_ is a function of all of its inputs. That is, this Derivation is specific to the exact combination of inputs that were used to create it. The same is true for every input in this Derivation's entire dependency graph.

### Instantiate

Our in-memory Derivation is serialized to disk at its own address in the Nix Store.

### Store Derivation

Now that the Derivation exists in the Store, any Nix Package Manager process attempting to Realize this Derivation or any of its downstream Derivations (those Derivations that depend on this Derivation's Outputs) now has a template that tells it how to do so.

### Realize the Derivation

Okay, now we actually want to run some binaries. So we Realize the Derivation, which forces it to be built. This will also recursively Realize this Derivation's entire dependency graph (skipping any that have already been Realized).

### Output

Now there is an actual (in our case) binary in the store that we can run!


## Summary

So, what have we shown here? Well, in particular, we've shown that you can build as many versions of a binary as you want, where the definition of "version" takes into account any "versions" of upstream libraries you may also be building from source.



[^technicallymodifying]: Okay, technically I am modifying `CFLAGS`{:.language-shell .highlight}, but only because that's how you define preprocessor macros at compile time. What I mean is that I'm not passing arguments that change how the compiler behaves; e.g.: `-O3`{:.language-shell .highlight}.
[^usebeforedefining]: I've already used some of these terms before defining them. I apologize for that, but it was hard to structure this document. 
[^derivationreturningfunction]: This Derivation-returning function is what is expected by the Nix Expression Language provided `callPackage`{:.language-shell .highlight} function, which we will be using in just a bit to kick off the build process.
[^inputbased]: This is known as "input addressing". If you want to go down a rabbit hole, contrast this with "content addressing" (which is currently being worked on for NixOS).
[^gcroots]: These symlinks are one instance of what is known as a Garbage Collection (GC) Root. There are different ways in which the Nix Package Manager creates GC Roots. Here, they are the output of the build process -- a convenience. More interesting are Profiles and Generations... the tools NixOS uses to manage a bespoke execution environment for every user on the system. Any Output reachable via an existing GC Root's graph will not be garbage collected when you run `nix-collect-garbage`{:.language-shell .highlight}. I wanted to talk more about GC Roots, Profiles, and Generations... but this post is too long already!
