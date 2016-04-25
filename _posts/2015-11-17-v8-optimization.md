---
layout: post
title: "Optimizing code for V8"
keywords: "javascript, browser, google chrome, node.js, v8"
category: blog
---

One of the things that I love to do the most is to take a complex system and
start digging through it to discover some subtle details that reveal how the
system really works. This writeup is about one of recent tasks where I wanted to
understand how to accelerate execution of some arbitary code for node. I coudn't
come up with better algorythm to solve the task but suspected that the code
itself could have been written in a better, more efficient way from
interpreter's point of view.

So, first I decided that I wanted to measure how fast the code was to be able to
understand if I do any progress at all. console.time api is a very good way to
do it:

~~~ javascript
console.time('mycode');

// ... code to be measured

console.timeEnd('mycode');
~~~

With this I can see if I do any better with the changes I introduce, the only
thing is that execution time is not deterministic and it always makes sense to
do multiple measurements and work with average. The other thing that can affect
the measurements is the way they are done. For example, V8 engine really
consists of two compilers - the former of them compiles source code to intermediate
code that can be executed, and the latter works with the code that is considered
to be _hot_ (executed many times through the program run) and compiles it to the
machine code. This has it's own implications, e.g. people like to do benchmarks
like this:

~~~ javascript

// ok, let's test the perf of implementation

for (var try = 0; try < 100000; try++) {
  superfunc();
}
~~~

As you see, the average execution time of this function can be affected by V8
implementation details. The other thing is that not all code can be optimized
and it may really matter how you iterate on arrays and objects what language
features are used and if V8 cannot optimize the code or cannot successfully
guess types of variables the code will suffer from additional performance penalty.

After all this, how can one gather the knowledge of such optimizations? A very
good source on this can be found
[here](https://github.com/petkaantonov/bluebird/wiki/Optimization-killers) or in
the awesome [blog](http://mrale.ph/) by Vyacheslav Egorov.

Additionally, profiling is always helpful and it is possible for node with help
of [v8-profiler](https://github.com/node-inspector/v8-profiler).


I started my investigation from the profiler results and saw a signigicant time
taken by garbage collector (50ms out of ~600) and a lot of anonymous functions.
The latter were useless and I went through all anonymous functions in the code
and added names to them to get meaningfull profiler report.

After some search I've figured out that the reason of the former was the use of
array map and reduce methods. When I replaced that with ordinary loop garbage
collector time reduced to 5ms on average.

Another speedup was with big object that came to be processed. On initial
implementation I created another and populated with the results. Two things
helped - reusing existing object and overwriting key values and using loop over
Object.keys() array instead of for..in loop.

Other discovery was that function .bind method is an optimization killer and
once I replaced `my_reg_exp.test.bind(my_reg_exp)` with `function(a) { return
my_reg_exp.test(a); }` in a code that was executed lot's of times I immediately
got big speedup.

Yet another speedup with iteration over big array came when I replaced a call to
the `Object.keys()` method with 'Object.getOwnPropertyNames()`. This method also
gives keys of the object but does not check the prototype and this is probably a
reason of speedup.

After all these changes I got 5x-6x acceleration in the execution time without
changing the algorythm or and data structures.

As a conclusion: I wouldn't to such optimizations on the code the is executed
only once throught the lifetime of the program or operates only on small amounts
of data but the fact the node can have such a drastic difference in run times
depending on such subtle differences really forces to keep this in mind when
dealing with the code that has strict performance requirements.
