---
layout: post
title: "On Common lisp"
keywords: "common lisp, programming"
category: blog
---

Though I have some experience with clojure which is lisp I always
had a feeling that this experience is incomplete without trying other
dialects, especially common lisp. This post describes my experience
on starting with this language.

I've read a couple of books, namely [Practical Common
Lisp](http://www.gigamonkeys.com/book/), [Land of lisp](http://landoflisp.com/),
[Full stack lisp](https://leanpub.com/fullstacklisp) and [Let over
lambda](http://www.letoverlambda.com/) and they all are beautiful. Macros part
is especially mind blowing because you simply don't think that way in most other
languages, and I still not used to them in a sense that I don't write my own.

After reading them I decided to give it a try and write something. The result
is my shiny new incomplete [cl-journal client for Livejournal.com
webservice](https://github.com/can3p/cl-journal). I set the following
requirements for the client:

* It should work with command-line
* Password should be stored in Mac OS Keychain or any other system level
  application, not locally
* The idea was that it should work as a pre-commit hook to replicate experience
  of static blog engines as much as possible.

While I found a solution for all these points, I was left with the feeling of
very fragmented toolchain and ecosystem. If we compare it to clojure world, all
project management and build tassk are done either with Leiningen or with boot,
and [clojuredocs.org](clojuredocs.org) is mostly enough to get all standard
library documentation and finally most libraries have decent looking
documentation pages. Every package has well-defined project file that contains
configuration, dependency information etc.

It's not like this in common lisp world (according to my limited knowledge).
Dependency management is done with the help of [Quicklisp](https://www.quicklisp.org/beta/),
project scaffolding is done with
[Quickproject](http://www.xach.com/lisp/quickproject/). Building is a whole
different story since common lisp doesn't build standalone binaries in usual
sense, here the way is to dump current state of lisp process (including compiler
and documentation) and define an entry point and run it whenever this image is
executed. There are several projects that aim to simplify building and running
applications, most notably [Rosewell](https://github.com/roswell/roswell) and
[Buildapp](http://www.xach.com/lisp/buildapp/). Rosewell looks really great, my
problem with it was that I haven't found a way to install scripts from local
repository, so I had to do manual symlinks here and there to make it work.

Since there is no central place for the documentation, I mainly used Practical
Common lisp book, [common lisp cookbook](http://lispcookbook.github.io/cl-cookbook/) and
[quickdocs website](http://quickdocs.org).

One of other unsolved issued for me was password prompt. I didn't find a proper
nonhackish way to do it with common lisp itself, so I had to run [external
command](https://github.com/can3p/cl-journal/blob/master/client.lisp#L13) for exactly this purpose.

For running commands I had to use
[uiop](https://gitlab.common-lisp.net/asdf/asdf/tree/master/uiop) library,
especially
[uiop/run-program](https://gitlab.common-lisp.net/asdf/asdf/blob/master/uiop/run-program.lisp)
and [uiop/os](https://gitlab.common-lisp.net/asdf/asdf/blob/master/uiop/os.lisp)
packages. Package there is more like a namespace  in clojure and clojure package
is a system in common lisp terms. uiop/os namespace was especially useful
because standard library doesn't provide any functions to control current
working directory.

Most of things described can be blockers only with first steps in the language
and I believe that once all they fit in head programming in common lisp gets not
harder than in any other language but with additional cool features.

The coolest one is of course an interactive development. Basically I've started
repl just once or twice during development, all the features were written and
tested in it and that had huge consequences for development process. If we take
any usual language, you work there by continuously running the whole program or
firing one-liners to get result from desired function. The drawback of former is
that if initialization step is costly, your development speed can suffer from it
a lot. The drawback of latter is that most of function cannot work in isolation - they
need specific datastructures initialization of global data object etc, and if
it's not necessary, developer still has to take care of proper printing of the
results. So, sometimes it's just to much of work to run things in this way, so
people revert to the first way. First way also suffers from the fact that instead of
implementing business logic developer spends a lot of time on boilerplate code
for passing parameters, entry functions etc. Interactive development eliminates
all that hassle.

My client had to load local database with information of
already published files before doing anything. Instead of loading it on every
change, I did it once and then simply tested new functions that I wrote with it.
And since I could do it, I did not implement boilerplate code till the very last
stage when I already had working business logic in place.

Interactive approach is very different also in sence that bottom-up approach
feels really natural there. E.g. In order to create a post I need to read a
file. How do I do that? (= new function). After I read, how do I parse it (= new
function). After I parse it ho do I understand that it's not a draft (= new
function predicate) etc etc. So I continued asking questions and putting my
answers as functions there till I get to very top where I wrote simple function
that really created the post.

As a result most functions are really small and
easy to reason about. You can go bottom-up in other programming languages, but
since you either have to write a lot of boilerplate code to get feedback or
right a lot of tests which really slow you down when you're just trying to find
a way to solve the problem.

If you ever did any clojure programming you know that it provides similar level
of experience. Where common lisp wins is a startup time for a built program
(instant) and human readable stacktraces that are not polluted with java classes
etc and excellent restart functionality instead of simple exceptions. Restarts
are super cool, because they don't just throw warnings, they also provide
options to recover and sometimes error can be just worked around with this.

As a resume - language is definitely worth looking at and using for real things.
I have very limited exposure to the language to talk about good parts and bad
parts, but a scattered and some times very brief documentation adds to a
challenge of learning the language. Many libraries that are widely used have
there latest release a few years in the past, and documentation is not always
nice (e.g. I had to read the code to understand how to use uiop packages
properly. The whole ecosystem has a few active members which means that not much
happens in a public space.

However there is a suprisingly big number of common
lisp implementations that are really high-performant and battle tested. I do
think that if language gets a proponent like Yehuda Katz, who will put effort
into popularisation of the language and systematic toolchain enhancements,
common lisp can get a great level of adoption.
