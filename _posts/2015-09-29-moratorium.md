---
layout: post
title: "Stop pushing the specs forward"
category: blog
---

Peter-Paul Koch wrote a [post](http://www.quirksmode.org/blog/archives/2015/07/stop_pushing_th.html)
a while ago, and there was a
[follow-up](https://www.nczonline.net/blog/2015/09/is-the-web-platform-getting-too-big/) from
Nicholas Zakas. While reading the first post I found myself almost agreeing with the author. Almost,
because I really think that the amount of new features is not the problem for me and I can explain
why, because the reason is very simple.

Throughout my career I work on high-traffic complex sites with lots of users and all that means that
all they use different browsers. From my perspective that means that I either have to use graceful
degradation for new features or just you the least common denominator of all specs, and in my case
it is Inernet Explorer 7 and 8.  It's not sexy, it's slow etc., but it's users still bring a load of
money and while version 7 is slowly fading away, version 8 will be with us for another year or two.
Another aspect is that we do a lot of small changes for the product and graceful degradation in pure
css just does not scale. It's relatively simple with the shadows or basic transforms, but anything a
bit more complex means that there is no graceful degradation for that, the solution requires
different markup for both cases. And it's not even about flexbox (which is btw not very new, first
spec was [released](http://www.w3.org/TR/css-flexbox-1/) in 2009!), it may be just
[calc](http://caniuse.com/#feat=calc) property and a very [big
number](http://caniuse.com/#compare=ie+8&compare_cats=CSS) if other spec that this browser doesn't
support. Even if we throw IE8 out, there is IE9 which is much better but still [not
perfect](http://caniuse.com/#compare=ie+9). There is an additional consideration: not all specs are
equally good. IE6 era was perfect in sense of true research. You remember all these sites with true
grids, bulletproof solutions for sticky footers and vertical alignment? One could imagine that all
this should result in one true layout specification that will be flexible enough to cover all them.
But it's not the case, people make mistakes, people make APIs that are not perfect. And this means,
that the thing that was once thought as solution for everything becomes obsolete artifact of the
past. I remember when `calc` property was introduced everyone was pretty sure that that's all we
need. Er, may be also `box-sizing` and that's it, all layout problems solved. And then we got
flexbox, [css grids](http://www.w3.org/TR/2015/WD-css-grid-1-20150917/), [css template
layouts](http://www.w3.org/TR/css-template-3/), [css columns](http://www.w3.org/TR/css3-multicol/)
and probably will get more. There are some specs that went out of fashion before they got
implemented by major browser vendors, like [css variables](http://www.w3.org/TR/css-variables/).
I am enumerating all this just to prove my point - at least in CSS part we use technologies that
are with us more or less since IE6 - floats, absolute positioning etc. And no matter how big the web
platform is we use only tiny fraction of it because it is the only one that works.

Another problem with new browser features is that they are all added in append-only mode. Features
can only come in, there is no way to take them out without breaking the web somewhere. And that
means that even unpopular or badly designed specs are here forever to annoy users and bloat
browsers. Time passes and now it's more and more clear that even cascade part of CSS was not the
best possible solution. And even more, if you ask me, what relatively new CSS specs are really worth existing,
I would mention media queries and transitions and this is it. But I can easily mention at least
two libraries types that really changed the landscape of styling - css grid libraries and css
preprocessors. There was no need to write a spec about them and implement them for evey vendor,
because these two technologies happily live on top of existing standards.

If we consider JS part of web platform it appears that the situation is a bit better in some parts
but fundamentally the same. Features keep being added, but we still have to send ecma 232/3rd ed. to
browser because that is the only version everyone supports. And again we are years behind from
latest and greatest and all innovation again happens in libraries - we have all sorts of languages
compiled to javascript, we have [transpilers](https://babeljs.io/) or even
[macros](http://sweetjs.org/) that give us the latest version of ES standard in any
browser without a need to implement all that in the browsers. Even module loaders happily existed as
external libraries for years before we got ES6 modules spec, and this spec still has whole bunch
of questions to solve - aliases, bundles, slices and a lot of features that existing module loaders
already have.

One would say that current fragmentation only exists because of IE, and once these guys figure out
how to quickly update their browser the problem will be solved. Certainly it will help, but mobile
fragmentation still exists and some browsers even start to implement -webkit- properties, because of
total dominance of different webkit based browsers in some markets.

So, my point is that new standards have a very long way to go before any wide adoption for natural
reasons and the only way to get through this mess is to abstract away and build something on top.
And this is the exact reason why we see a real explosion of influentual libraries like Bootstrap or
React.JS. They just work for majority of cases, they fix big problems now without the need to wait
for indefinite future. But do I want to see them or their approach to be adopted in browsers
as a spec? No, because that will kill the major benifit of the libraries - you can use them as long
as they fit the purpose and you can throw them out if something better appears on the market.

So, what is the conclusion of all this writeup? I think that the future is not after new platform
extensions, but in libraries that will provide standard ways to solve standard problems. Dialogs,
lightboxes, calendars, layouts, endless battles about module formats, server communication  - that's
what bothers most of people and wastes billions of work hours and solving these issues will push the
web forward for real. And there are [plenty](http://elm-lang.org/) of
[more](https://github.com/clojure/clojurescript) or [less](https://www.dartlang.org/)
[successful](://www.meteor.com/) attempts to solve these issues.
