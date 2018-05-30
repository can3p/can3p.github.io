---
layout: post
title: "On writing a blog client in Common Lisp"
keywords: "common lisp, livejournal, blog, cl-journal"
category: blog
---

I love markdown and vim. I do blogging and I am fed up with interfaces.

## Intro

**Disclaimer** Please treat this post as an experience report more than
anything else because this is what it is. As a consequence there can be a lot
of things real lisp wizards will raise an eyebrow on.

Below you will read a story about writing a blog client for livejournal.com
webservice in Common Lisp. As one may note, it sounds like using a very niche
language for a near dead web service. To make you a bit more surprised I can
add that I've been working on it on and off for a couple of years now.

I've been using Livejournal since 2005 and never had a reason to switch to
anything else. I almost never write on political topics and don't really care
where it's hosted. I don't even need many connections but do value some and the
fact that livejournal provides near zero way to discover other blogs sometimes
works more as a benefit than a drawback, because with current state of things
when majority of users migrated to Facebook or Instagram, there is not that
much to read there. It's not engaging as Facebook, and that's *good*. The other
benefit of the service is that it's a very good site to write long posts like
this and people actually do that and there is a ton of beautiful content there,
if you're lucky to find it of course.

My main problem with livejournal always was that I didn't really own the
content in sense of storage. Blog is mine, ok, but if webservice goes down or
account gets blocked I'll lose all my ~1400 posts written to date without any
good way to get them.

From time to time I tried to use other blog platforms or static site generators
(like this site) and my real dream was to combine them and have sources in
markdown locally, so that I can edit them in my favorite editor and still
publish them on a platform that has at least some social flavor. Blog on github +
disqus is not social for sure.

I had this idea for some time and somewhere around 2016 got really burned out
by trying to ship yet another webservice in my spare time in addition to those
that I ship for my company. To recover I decided to go in the direction that
absolutely excluded any interest except my own and didn't have any money or
fame in sight.  You see now, Common Lisp and idea about better blogging at the
time where blogging is already out of fashion is almost a perfect match! And
this is where it all started.

## Idea

What does perfect blogging experience look like? It's when you write the text
in your favorite editor, save it, push it to git and it's published. And it's
when you can just grep your posts to remember something from the past or when
you can do buk updates to all the posts with system tools that you're used
too and get all changes published at once.

This was my thought process and this is what became a base of requirements.
Client should be able to:

* See new markdown files to publish
* See modified markdown files to submit changes
* See deleted markdown files and be able to delete them in the service

Given that I imagined two abstractions - one for a post file and one for a post
itself.
