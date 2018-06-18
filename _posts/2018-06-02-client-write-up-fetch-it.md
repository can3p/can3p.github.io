---
layout: post
title: "Part 3: Fetch it!"
keywords: "common lisp, livejournal, blog, cl-journal"
category: blog
tag: building-blog-client
---

All the functionality mentioned before was quite complete and I used
it constantly for a year or even more. There were two drawbacks,
however:

* I had source code only for the posts I wrote using the
  client. Nothing from earlier days was preserved and in case
  livejournal.com goes down forever, I would lose them all.

* All the posts had to be written and update later with the client,
  because it had no way to know about changes done elsewhere.

Both parts were not important for day to day writing activities, but
the former kept returning after every next negative news about the
service and the latter was more of a challenging task that I wanted to
tackle. I resisted for a long time but finally gave up and decided to
write the logic. Besides that, how could it be a real git like client
if it has `push` command but didn't have `fetch` or `merge`?

For the first question `fetch` was the most important bit. I assumed
that if I downloaded posts in any form or shape I could work later to
turn them into a markdown later without any hassle.

Luckily original Livejournal authors assumed such needs and designed
API protocol to enable it (Kudos to Brad Fitzpatrick!).

First important bit is [syncitems][syncitems]. What it allows to do is
to get updates since some timestamp or from the beginning. A change
can be a new post, an update to post or a comment, and since any post
could be updated several times, duplicates were expected. The only
thing protocol did not support were post deletions, which meant that I
wouldn't notice such a change.

I didn't care about comments or deleted posts, however, what I wanted
to get was a list of posts that changed somehow since timestamp. To
make things even more complicated, api call didn't return **all**
changes but just some subset of it and I had to do several calls in a
row to get the desired result.

Logic looked quite complicated and I ended up literally
[writing][sync_logic] the steps algorithm should take and them slowly
implementing them step after step and surrounding them with tests to
keep it under control, you can check the final logic in
`get-fetched-item-ids` function in [lj-api][lj-api].  As you can see I
used `-<>` threading macro all over the place to keep the code
readable. Apart from the logic itself, I ended up implementing some
function helpers that I didn't find in the language but that we're
very useful.

The first example of this is `acc`. I do a lot of Perl and data traversal
is really good there. What the language allows you to do is to easily
access nested arrays without checking the existence of anything -
`$obj->{a}{b}{c}`. Common Lisp `getf` is much less flexible and allows
only one level lookup and this is where `acc` is useful since it
allows any series of keys:

```common_lisp
(defun acc (l &rest args)
  "Access member in a nested plist.

   Usage (acc l :one :two :three)"
  (cond
    ((null args) l)
    ((not (listp l)) nil)
    (t (apply #'acc (getf l (car args)) (cdr args)))))
```

Another two functions are `partial` and `print-and-return`. Name of
the first is self-explanatory and the next one accepts a value, prints
it and immediately returns it. This is useful in the middle of
threading macro call, because it allows printing intermediary steps in
the computation.

One last perlism is `to-hash-table`. In Perl transformation between a
list and a hashmap is extremely simple and happens all the time. This
is how I would do it:

```perl
my %ht = map { $_->{id} => $_ } @list;
```

This simplicity means that most of Perl code consists of jumps between
lists and hashes to use whatever works best in a particular situation.
Posts are stored in the database as a list hence I had to iterate over
it in a more or less smart way all the time. A list is a reasonable
structure there and one of the ways to fix the problem with lookups was
to maintain a hashtable in the class, but I I decided to go with a
simpler one and convert the list to the table on the go.

```common_lisp
(degun to-hash-table (l)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (item l ht)
      (setf (gethash (car item) ht) (cadr item)))))
```

This snippet highlights one of the neat features I like so much in
Common Lisp. Maybe it has not the best possible standard library in
the world, however, there are true gems of usability there. In this
particular case, `dolist` accepts the third parameter which will be
returned as a result of dolist. That means that you can write things
like filling in hash table very naturally.

After `get-unfetched-item-ids` function was done, the next one on the
list was one to download a list of posts. And here it got a bit tricky
because of Unicode.

### Unicode and Livejournal

Livejournal is there for quite a long time and that means that it's
codebase and data predates the times when everything became all
Unicode.  Many veteran coders can still show scars from that time,
yeah. In essence, there was an ASCII table that was used to define a
meaning of the byte of data (hence single byte encoding) and it
defined the meaning of the first 127 values and left the rest
undefined ant that was used as a space for extra characters for every
specific alphabet. In Russia, there were two most popular encodings -
cp1251 and koi8-r. How did it affect Livejournal? As is written in
their FAQ, they had no way of understanding the encoding and hence it
was left up to users to choose a proper one to render a page.

In case such encoding was chosen, Livejournal API allowed to download
both Unicode and non-Unicode posts in the same way. Unfortunately,
what I found empirically, serverside encoding did some weird stuff
presumably because the text was already converted to Unicode somewhere
along the way, and I had to disable it in order to get a properly
readable way.

This decision had consequences in a sense that now I couldn't download
different types of posts in one batch - Livejournal api returned an error
in this case. The beast I ended up writing is called
`lj-getevents-multimode` you can check it out in [lj-api][lj-api]. I
baked in a couple of assumptions in the code:

* One of the api versions (Unicode one) was much more probable
* If the post had one version, the next one had a high chance of having
  the same one.

Final logic looked like this:

* Try to download posts in one version
* If fails reduce batch to one post and repeat
* If that fails, flip download mode and repeat
* If that fails, error out
* If one of the previous steps succeeded, increase batch size 2x and
  repeat.


Maybe a bit naive approach, but it worked really well in the
end. After these two bits were done, fetch logic started looking very
simple:

```common_lisp
(defmethod fetch-posts ((store <store>))
  "Fetch all new items from remote service since last-fetched-ts
   of the store"
  (multiple-value-bind
   (new-itemids last-item-ts ht) (get-unfetched-item-ids store)
   (cond
     ((null new-itemids) store)
     (t (let ((new-events (-<> new-itemids
                             (lj-getevents-multimode)
                             (getf <> :events)
                             (mapcar #'(lambda (x) (enrich-with-ts x ht)) <>))))
          (merge-events store new-events last-item-ts)
          (fetch-posts store))))))
```

`<store>` there is another class I created to store a list of
downloaded posts in their original form. `enrich-with-ts` function
exploited the fact that `get-unfetched-item-ids` could now server
change dates for any updated post and download post api call did not
return that and it simply added such a timestamp to every
post. `merge-events` did no more than placing downloaded posts at the
end of the list.

Now, where should I take this store? I mimicked the way I stored and
saved the database with posts and added a hacky solution to do lazy
loading of the database. I don't think it's necessary since posts are
downloaded differently, but in case you wonder here is the
implementation:

```common_lisp
(defmethod slot-unbound (class (db <db>) (slot-name (eql 'fetch-store)))
  (setf (slot-value db 'fetch-store)
        (make-instance '<store>)))
```

This gets triggered whenever slot doesn't have value set and function
sets slot value after creating the class instance. Next time slot
already has value and this method is not called anymore.

Top level fetch function now looked very simple:

```common_lisp
(defun fetch-updated-posts ()
  (let ((store (restore-source-posts (fetch-store *posts*))))
    (fetch-posts *posts*)
    (save-source-posts store)))
```

After I got all this working I got a raw dump of all the posts I ever
wrote, which meant that I could safely work on actually converting
them back to markdown without fear to lose the contents.

## Testing

To be frank sync protocol didn't come for free to me. Too many moving
parts and conditions. And while most of the code base has been written
in a pure leisure fashion without a single test, I decided to build
new features starting from fetch with at least some coverage. And I
chose `prove` as a framework of choice.

The most annoying bit of this framework is it's default reporter which
uses escape control sequences for colors and emacs requires some
additional configuration to make that work and that's not something I
wanted to invest my time in. Instead, I invested my time in finding a
way to disable them. It appears that I've always been one dynamic
variable away from the result:

```common_lisp
(setf prove:*enable-colors* nil)
```

The overall tests integration could have been easier, however, still
doable.  I've made a test system, added a magical spell to the main
asd file and test framework was set up. One of the important things to
note is that prove itself is a dependency of the test system, hence in
order to have it available in the repl this test subsystem should be
loaded instead of the main one which will be loaded as a dependency.

A really awesome feature of the `prove` framework is that it allows to
rerun specific tests just by recompiling them. This feature enables
near magical workflows when I could prototype a feature and then cover
it with tests in real-time without the need to run a full test suite
again and again or to do a build every time and run tests there.

Since I wanted to write tests for the logic built around api calls I
wanted to mock them to test outcomes of specific sequences of calls.
I took `mockingbird` and it did provide me with a basic feature of
mocking any function in any package, however, I ended up implementing a
small macro to enable testing a sequence of calls.

Funnily enough, all the mocked calls ended up being trivial, but the
possibility is still there! The idea was that if you want to mock a
function, say `foo` and have it return `(1 2 3)` on the first call and
`nil` on any subsequence you could just write:

```common_lisp
(with-mocked-calls
  foo
  (
   (1 2 3)
   nil
   )
   (some)
   (code)
   (block))
```

Macro allows generating a lambda function for mockingbird that has a
baked in logic that tests against the number of calls and returns a
respective result. Here is it:

```common_lisp
(defmacro with-mocked-calls (func data &rest body)
  "This function is necessary to emulate the behavior of
   a function that has side effects. Every subsequent
   call to the function will return next item from the
   data list, except the last one which will be returned
   endlessly"
  `(with-dynamic-stubs
       ((,func
         (lambda (&rest rest)
           (declare (ignore rest))
           (cond
             ,@(loop for resp in data
                    for i from 1 to (length data)
                    collect
                    (if (equal i (length data))
                        `(t (quote ,resp))
                        `((equal (call-times-for (quote ,func)) ,i)
                          (quote ,resp))))))))
     ,@body))
```

I'm no macro guru and I get super excited about every single case when
I managed to write something that actually looks like useful
thing. You can check the [tests][cl-journal.t] for this definition and
real-world usage.


[roswell script]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/roswell/cl-journal.ros
[buildapp script]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/Makefile
[main package]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/main.lisp
[cl-brewer]: https://github.com/can3p/cl-brewer
[cl-journal]: https://github.com/can3p/cl-journal
[magic-ed]: https://github.com/sanel/magic-ed
[xml-rpc]: https://www.livejournal.com/doc/server/ljp.csp.xml-rpc.protocol.html
[s-xml-rpc]: https://common-lisp.net/project/s-xml-rpc/
[rpc4cl]: https://github.com/pidu/rpc4cl
[cl-arrows]: https://github.com/nightfly19/cl-arrows
[lj-api]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/lj-api.lisp
[db]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/db.lisp
[file-api]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/file-api.lisp#L35
[markdownify]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/markdownify.lisp
[cl-journal merge]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/markdownify.lisp#L243
[markdown]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/markdown.lisp
[main]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/main.lisp
[syncitems]: https://www.livejournal.com/doc/server/ljp.csp.xml-rpc.syncitems.html
[getevents]: https://www.livejournal.com/doc/server/ljp.csp.xml-rpc.getevents.html
[sync_logic]: https://github.com/can3p/cl-journal/commit/93695d3b0de4a9cdb37ee7b79a30de5bd2ed0370
[cl-journal.t]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/t/cl-journal.lisp#L96
[ljprotocol]: https://github.com/apparentlymart/livejournal/blob/master/cgi-bin/ljprotocol.pl

[reddit slugify]: https://www.reddit.com/r/Common_Lisp/comments/67neph/clslug_slugify_uris_camelcase_remove_accentuation/
[blog repo]: https://github.com/can3p/can3p.github.io/issues
[cl-journal repo]: https://github.com/can3p/cl-journal/issues
