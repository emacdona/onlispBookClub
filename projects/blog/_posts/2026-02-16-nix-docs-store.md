---
layout: post
published: false
title: "A Nix Store-y"
---

# A Nix Store-y

Prior to the date on which this post was published, I had secretly committed to using NixOS as (one of) my daily driver(s). Now that has changed: If you are reading this... well... it's no longer a secret.

If this all goes well, there may be future blog posts on home-manager, nix flakes, the nix language, and perhaps just general fawning over the concept of treating your entire operating system as a collection of immutable packages with immutable dependencies upon one-another.

But you'll have to wait for those blog posts. Today, I want to talk about a very specific part of their documentation.

## Mathematicians

I have a batchelors degree in math, but I want to be clear: I do not claim to be a mathematician. I just mention it because I do have at least some experience operating in an environment where mathematicians are moulded.

In particular[1], I've read lots of pages of lots of math texbooks, and had plenty of disucssions about abstract mathematical models. 


Nix Store: A flat datbase of Store Objects.
Store Object: A hierarchical database of Filesystem Objects.

Both of which happen to live on a filesystem, one (both) of which happens to use specially crafted filesystem paths as its (their) addresses