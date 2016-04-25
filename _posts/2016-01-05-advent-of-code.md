---
layout: post
title: "Advent of code quiz reflections"
keywords: "advent of code, clojure"
category: blog
---

This December a big chunk of my free time was spent on
solving quize from [Advent of code](http://adventofcode.com/)
challenge. While I wasn't anywhere amongst the leaders
it was a pure joy to see how clojure helps to gradually
build solutions without constant switching between the editor
and console.

If the only thing you have is a hammer then everything looks
like a nail and majority of tasks ended as some sort of list
generation which is somehow reduced to the to the answer value.
If you look at the task from this point of view you will discover
that it really helps to avoid most of side effects or temporary states
which can be a big source of bugs.

If you think about everything as list it becomes immediately evident
how rich and cool core library is. Apart from it I used only
[clojure.string](http://clojuredocs.org/clojure.string),
[clojure.set](http://clojuredocs.org/clojure.set) and
[math.combinatorics](https://github.com/clojure/math.combinatorics) namespaces.

The main disadvantage for me was that with certain number of
map/reduce/filter functions in one expression code becomes much
harder to read for anyone who is not very familiar with them
and sometimes it's evident that in some cases types can really help
because it's hard to say how structure looks like at given point
of transformation. May record is 9 map/reduce calls in one function.

The other pain point is emacs cider and changing project dependencies.
Maybe I don't read documentation correctly but the only good way to
load new dependencies is to restart repl and to do that I had to kill
it. Sad news is that after that in most cases I ended up with restarting
emacs which meant losing undo history and repl logs.

Ok, back to interesting stuff. I want to share a few incredibly useful
functions from the core library that I discovered while doing the challenge.
Although simple sometimes they drastically simplify the code.

###  [reductions](http://clojuredocs.org/clojure.core/reductions)

Used in [day 1](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day1.clj)

This function takes the same arguments as reduce but instead of final
value returns a sequence of all intermideary reduction states. It's
useful if every reduction step has it's own meaning, e.g. distinct
state that you want to analyze. Or if you have an infinite sequence
you can use reductions to count the steps to get to the desired state.

### [every-pred](http://clojuredocs.org/clojure.core/every-pred)

Used in [day 5](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day5.clj) and [day 11](https://github.com/can3p/adventofcode2015/blob/e2b130ee64d9a638554238414dc209a3ce8081b6/src/advent_of_code_2015/day11.clj).

Takes any number of functions and returns new function that will return
true only if all the functions return true with arguments passed. Very useful
to write conditions in compact way:

~~~ clojure
(def ans1 (count (filter
                  (every-pred has-wovels
                              has-double
                              has-good)
                  (str/split input #"\n"))))
~~~

### [partition](http://clojuredocs.org/clojure.core/partition)

Used in [day 5](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day5.clj), [day 11](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day11.clj) and [day 13]([day 5](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day13.clj))

Function allows to get a sequence of list every of which is a
subsequence from original of some width and with some offset from
the begining. In my case it was very useful to check any connections
between sequence items.

Here is an example:

~~~ clojure
(def chmap (apply hash-map
                  (flatten (partition 2 1
                           (concat (char-range \a \z)
                                   (list \a))))))
~~~

This expression generates a hashmap with keys that are individual characters
and the values that are the charecters that alphabetically follow them, except
a follows z. partition takes (\a \b .. \z \a) list as input and returns
sequence like ((\a \b) (\b \c) ... (\y \z) (\z \a)). After that I transform
this sequence to the flat list and feed it to hash-map function.


### [group-by](http://clojuredocs.org/clojure.core/group-by)

Used in [day 12](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day12.clj)

Groups sequence according to return values of predicate.

### [iterate](http://clojuredocs.org/clojure.core/iterate)

Used in [day 18](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day18.clj) , [day 17](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day17.clj) and [day 25]([day 5](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day25.clj))

Function takes value and another function and returns a sequence that starts from
the value and continues with applying function to previous sequence item to generate
next one. Helps to avoid loops in all situations where you need to build next value
on top of current one.

### [re-seq](http://clojuredocs.org/clojure.core/re-seq)

Used in [day 19](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day19.clj), [day 21](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day21.clj), [day 22](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day22.clj), [day 23](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day23.clj) and [day 25]([day 5](https://github.com/can3p/adventofcode2015/blob/master/src/advent_of_code_2015/day25.clj))

Works like re-find but returns a lazy sequence of all consequtive matches.
