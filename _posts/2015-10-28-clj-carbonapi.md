---
layout: post
title: "clj-carbonapi internals"
category: blog
---

A while ago I decided to implement something close to the real world in clojure.
The easiest way to do that was to take some existing non-toy project and try
to replicate it. My choice was to reimplement parts of
[carbonapi](https://github.com/dgryski/carbonapi) and the result is called
[clj-carbonapi](https://github.com/can3p/clj-carbonapi). The overall experience
was so good that I decided to write a blog post with finding and pitfalls
encountered during the implementation.

The first and the biggest finding is that REPL provides absolutely life
changing experience and any other development process feels like a bleak shadow
regarding the speed of prototyping and development especially if we talk about
nontrivial code. For me the truth is that for complex tasks I sometimes don't
have any idea about how implementation should look like, what data structures
should be used, what is the correct way to use new library etc. and combination
of decent editor and a repl (Emacs + Cider in my case) allows to start writing
some code that can be immediately tested in the repl and start building complex
solution from the small pieces without a need to implement boilerplate code
that should be written in any other project just to start doing something. In
my case I started from implementing parser, then moved to the fetching part,
added functions support after that and finally implemented graphs, server and
cli interface.

As you can see, usual points that sound like first steps in most
languages, just because you you need them to start seeing any results are not
really necessary in clojure. When I was working on any of parts I was able to
quickly iterate on the results and make things work piece by piece from small
to big until I get everything right.

Having said all that I want to admit that clojure repl is not a replacement of
unit tests, it's just super cool prototyping tool. The pro of it is that you can
test things continuously without any additional code to support and anyone who
wrote anything with TDD techique knows that units test take a lot of time to
support especially when you aim for full coverage. From the other hand repl is
not a code that you can save in the repo to rerun later and tests are still
needed after implementation is ready because at this moment it's important to be
sure that things will keep working as expected. In my case I wrote
[test](https://github.com/can3p/clj-carbonapi/blob/master/test/carbonapi/parser-test.clj)
for parser immediately when I decided that I covered all the possible syntax
scenarios. The other benefit of the repl comparing to the unit tests is that you
can actually run function from any namespace you have to get an idea how it
works. Final benefit of the repl that I want to admit is the fact that you can
use any functions to test results of the function call and that mean that you
can play with the data with all the functions available to the core and then
write real code when you're confident with the path you want to take.

The application is split into several modules and most of them are surprisingly
small. Here are the few notes on each.

## carbonapi.parser

Parsing is one of the tasks that can be harder or easier depending on whether
you know the right way to solve it. For parsing the right approach is to use
context free grammars and clojure ecosystem provides beautiful
[instaparse](https://github.com/Engelberg/instaparse) library for this purpose.
Library takes a grammar that can be written in one of two languages, compiles it
and uses it to parse a string into nested list and also supports
transformations, so it's really easy to get the proper structure.  I won't go
into the details here, because there are already
[some](http://www.walkwithoutrhythm.net/blog/2013/05/16/instaparse-parsing-with-clojure-is-the-new-black/)
[tutorials](http://gigasquidsoftware.com/blog/2013/05/01/growing-a-language-with-clojure-and-instaparse/)
on the net.

The second interesting part is actual tree execution. While it's quite possible
to make a nested list that can be then evaluated there is a problem with this
apptoach - all these functions won't have any context in them and that means
that AST should contain all input data (or have side effects) to make it
possible to execute it with eval.

This issue is solved with postwalk function from clojure.walk package. Function
takes a nested list and a function as an input and calls the function with the
lists stored in the source list starting from the most nested one and goes up to
the root replacing parts of the list with results of the function call. Thus
it's relatively easy to eval list manually with any logic.

## carbonapi.functions

carbonapi.functions namespace provides functions to work with cabonapi metrics
objects. The interesting part was that at least in simple cases I could abstract
way metrics object format and allow to define operations on the data with a
[simple
function](https://github.com/can3p/clj-carbonapi/blob/master/src/carbonapi/functions.clj#L47)

I didn't implement complex cases but I think that this is not a problem of
complex algorythm but just a matter of implementation.

## carbonapi.remote

This namespace provides an interface to fetch data from carbonzipper or graphite
instance. The json is implemented in a quite straightforward way, the most
interesting part is in protobuf support. The complication with it is that
protobuf has it's own minilanguage and a compiler that turns this language in
the source code in java. While definition itself was
[found](https://github.com/dgryski/carbonzipper/blob/master/carbonzipperpb/carbonzipper.proto)
in carbonzipper repo, the integration part erased few evening hours from my
life.

The main problem was that in clojure there are two packages needed for this -
[clojure-protobuf](https://github.com/ninjudd/clojure-protobuf) and
[lein-protobuf](https://github.com/ninjudd/lein-protobuf) and you can be as
unlucky as I was to choose incompatible versions of these two packages and get a
compiled java file that doesn't play nicely with library. So, I warned you.

The other tricky thing with protobuf was that for some reasons I couldn't see
all the data in a hash returned by protobuf-load and I had to implement a
function that took response parsed from protobuf input and transformed it into
the structure in a format returned in json format. After inspection of compiled
java file I found out that there is a method valAt that [can be
used](https://github.com/can3p/clj-carbonapi/blob/master/src/carbonapi/remote.clj#L14)
to the data that is hidden otherwise.

## carbonapi.chart

This part was done thanks to [incanter](http://incanter.org/) library and
[JFreeChart](http://www.jfree.org/jfreechart/) library that powers plotting
functionality of incanter.

I once again had to massage data before it could be passed to the plotting
library but otherwise the library was a pleasure to work with. I easily got a
graph in a new window (which is super useful in a repl usage) and at the same
time I could get and image object that can be processed and
[sent](https://github.com/can3p/clj-carbonapi/blob/master/src/carbonapi/server.clj#L18)
by server to the client browser.


## carbonapi.server

I used [http-kit](http://www.http-kit.org/) to implement server and the only
note I can leave that it was easy and straightforward and integration with
charts was suprisingly simple and painless.

## carbonapi.core

This is a namespace that provides a top level api to show graphs and is supposed
to be an obvious entry point for any repl usage.

## carbonapi.main

This one is obvious entry point for cli usage. The cool part is the usage of
clojure.tools.cli package that is the greatest cli parsing library I ever made.

## Conslusion

Well, it was fun!

