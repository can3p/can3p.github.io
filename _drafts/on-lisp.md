---
layout: post
title: "On Common lisp"
keywords: "common lisp, programming"
category: blog
---

Looking on a programing languages landscape it's possible
to see different camps of the languages. Borders are blurry
of course and camps can have different names for every
programmer, but still, one can always say that C# and Java
are close to each other because they are kinda *industrial*
languages (read: big corporations invested a lot into them
and control ecosystem), there are *cool* languages like Rust,
Clojure and to less extent Javascript and there are *practical*
languages that are used for myriads of different tasks for a
very long time but are not the latest buzzword and some of them
even tend to get out of fashion completely eventhough
there are extremly useful in their application field. Perl is
a very nice example, Python and C/C++ are the others.

There is last group I didn't mention - I'd call them *mystery* languages.
Everyone heard of them, some read about them, few tried them and
almost no one saw I living human being coding in any of those languages.
At the same time they have some legendary status because they are
so different and they were a real thing decades ago. I'm talking
about such languages as common lisp, smalltalk and prolog now.

All these languages had a very big influence on new ones and suprisingly
they remain as a source of new ideas till this day. My preferece there
is lisp family, because of fantastic flexibility and expressive power.
Once I read a book about clojure I understood that my life won't be the same
any more. The language was designed so good that my usual languages
of choice like javascript seemed more like a pale shadow.

I spent some time doing projects with clojure and it was always a good experience -
friendly, active and growing community, a lot of innovative libraries
that are also very solid, great interoperability with java.

Yet after some time I became really curious about difference between
clojure and other lisps especially common lisp. Finally I gave up and
decided get some experience with this language and this post is a summary
of my experience with it. Notes can be a bit fragmented, just because I keep
updating post when there is something to add.

## General impressions

General impression can be summarized as following

- Very small community. Just have a look on relevant subreddits to get
a feeling on how active it is.
- Not very novice-friendly community. When new people come to language they
expect to get some answers on dumb questions and I encountered lots of threads
where the answer given is smart maybe a bit arrogant but not helpful.
- Almost nonexistent job market. Even very extensive search revealed me just
two companies that use common lisp at the moment. Most of other examples
are really dated and are not proved by relevant sites
- Most of libraries are old. Another evidence on how small community is. Most
libraries I found to use had latest release several years ago. Let's have
a look: (s-xml-rpc)[http://quickdocs.org/s-xml-rpc/] - latest release in 2010,
(cl-markdown)[http://quickdocs.org/cl-markdown/] - 2010, (md5)[http://quickdocs.org/md5/] - 2015 which is very recent.
- Very fragmented documentation. I mostly used (cl cookbook)[http://cl-cookbook.sourceforge.net/], quickdocs.org and (Practical Common Lisp)[http://www.gigamonkeys.com/book/] book.

If we're talking about ecosystem, then:
- quicklisp is great, but every project still has to have several files to describe
dependencies
- Some things are really hard to do. Try to make a password input in console or
execute a shell command. I really started wondering if I desire something unnatural, but
it doesn't seem to be the case.
