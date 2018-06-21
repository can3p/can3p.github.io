---
layout: post
title: "Part 4: Merge"
keywords: "common lisp, livejournal, blog, cl-journal"
category: blog
tag: building-blog-client
---

With a database of raw posts at my hands, I started thinking about ways
to implement the merge. First and foremost I had to identify which
posts actually had to be merged. This is trivial with missing posts,
but for existing ones I had to have something to compare them to, and
since I had raw posts from the server with the server timestamp I had
to get server timestamp for all existing posts and update it for any
posts that I wanted to change.

If you remember I already had `ignore-all` command to bump local
modification timestamp of all posts and I ended up doing the same for
the remote timestamp. The easiest way to get it was to take it from the
`syncitems` call and to crossect them with all existing posts.  As a
first step, I ensured that every fetched post received corresponding
`syncitems` update timestamp and after that I only had to add this
timestamp to all the posts in the database.

And this is where my obsession with `loop` macro began.

```common_lisp
(defun mark-as-pulled ()
  (let ((ht (-<> *posts*
                 (fetch-posts)
                 (restore-source-posts)
                 (to-hash-table))))
    (loop for post in (posts *posts*) do
      (let ((ts (gethash (itemid post) ht)))
        (if (null ts)
            (format t "Item ~a was not fetched yet, run cl-journal fetch first~%" (itemid post))
            (setf (server-changed-at post) ts))))
    (save-posts)))
```

You see there old friends - `-<>` macro and `to-hash-table` function
which became independent implementation there with an option to chose
what value to use as a key for a hash table. Once that was done update
became as trivial as setting slot values for any posts that were found
in the hash table.

That is one direction, but as I said there was another one - I had to
get a server timestamp for any post update or creation I did. The post
data being returned contained some timestamps but none of the public
ones contained a timestamp of last post update. I started looking
elsewhere and found that `getevents` call also returned server
timestamp.  Super handy! I did a quick function to get a timestamp
based on the call only to find out later that timestamp did not always
match the one from the `syncitems`.

What the hell? Timezones. Livejournal api is quite naive in terms of
dates in a sense that all the timestamps it returns are in format
`YYYY-MM-DD HH:MM:SS` and one can only assume that they all relate to
the same timezone and `getevents` call was one particular example
where this invariant didn't hold, or to say it more precisely, not
always.  It seems like this api call is served by the two servers in
two different dcs with their local timezone set. `syncitems` seemed
to work with UTC timezone and I started looking around in search
of something that can work. First solution I came up with was
rather horrible: we just need to keep calling `getevents`
till we get two different timestamps and from that point we can take a
lower one and save it along the post.

```common_lisp
(defun lj-get-server-ts ()
  ;; scary hack to get server ts in a single timezone
  (labels ((r () (getf (lj-getevents '(1000000)) :lastsync))) ;; something big enough to have empty lookup
    (loop with a = (r)
          do
             (let ((b (r)))
               (format t "~a~%" b)
               (when (older-p a b 10) (return a))
               (when (older-p b a 10) (return b))))))
```

You see? It's `loop` again. What happens is that we keep calling the
api and keeping the result of the previous call. Once we see the
difference we return the earliest. Loop works wonderfully there: it
allows to set a local binding `a` and then execute loop body and
return from it with `return`.

`older-p` is also interesting. I didn't find a simple way to compare
`YYYY-MM-DD HH:MM:SS` timestamps, however, [local-time][local-time] library
provided a way to compare it's time object instances. And this helper
function exploits this fact:

```common_lisp
(defun older-p (ts1 ts2 threshold)
  (labels ((parse (ts)
             (let
                 ((local-time::*default-timezone* local-time::+utc-zone+))
               (parse-timestring ts :date-time-separator #\Space))))
    (timestamp>
         (timestamp- (parse ts2) threshold :sec)
         (parse ts1))))
```

Setting timezone doesn't have any special meaning there, it's just
some stable value that allows us to compare timestamps. The threshold
is there to filter out small inconsistencies between time stamps.

While it worked at the beginning, later I was punished for my
naiveness and `getevents` started returning mostly non UTC datetime. I
decided to search more and finally understood that I could as well
just use unix timestamp that some of the calls return and then format
it according to timezone of my liking. `local-time` library was really
helpful in this sense. Timestamp formatting can be implemented as easy
as:

```common_lisp
(defparameter *livejournal-timestamp-format* (list
                                 :year
                                 "-"
                                 (list :month 2)
                                 "-"
                                 (list :day 2)
                                 " "
                                 (list :hour 2)
                                 ":"
                                 (list :min 2)
                                 ":"
                                 (list :sec 2)
                                 ))

(defun format-unix-timestamp (ts)
  (format-timestring nil (unix-to-timestamp ts)
                     :format *livejournal-timestamp-format*
                      ;; looks like utc zone is what syncitems use
                     :timezone +utc-zone+))
```

Initially I used `defconstant` there, but every copilation resulted in
the restart and sbcl even [mentions][defconstant] this behaviour.  I
gave up and moved on to `lj-get-server-ts` function that now looked
much simpler:

```common_lisp
(defun lj-get-server-ts ()
  (->
   (rpc-call "LJ.XMLRPC.getchallenge")
   (getf :server_time)
   (format-unix-timestamp)))
```

After this was done I had a clear way to get posts to merge and it
appeared to be an interesting exercise! Fundamentally speaking there
are two parts: first, we need to map all the fields supported by the
client to the fields in a raw response, and second, we need to take
post body from the response which happens to be html most of the time
and convert it into markdown.

### Fields

Livejournal API is really thought through for it's age and has a lot
of decisions make it handy for the clients. One example is a changelog
provided by `syncitems` call. The other one is that although it's not
REST or anything alike original authors already thought about entities
and it appeared that fetch api returns more or less the same structure
that post api accepts. More or less means that main fields are on the
same places, however, reading entity returns more data and most of it
was added much later and is not documented anyhow. Another important
bit that whenever field contains non-latin characters it's accepted as
is for posting but is returned encoded in base64.

I didn't know or care about this similarity when I wrote posting part
and didn't spot it right away when I did reading part and that led to
the existence of two different pieces of code to handle fields. About
the first part I already wrote, see the description of `to-event-list`
generic. Reading part is handled by `parse-xml-response` in
[file-api][file-api]. The idea of this function is to return parsed
data in such a way that I can use it later to create instances of
`<post-file>` of `<post>`. Potentially non-latin fields are handled by
`b64getf` function that works almost like `getf` however, if received
value is a list with car equal to `:base64` it takes cdr and decodes
it.

### Markdown

Now, one more interesting part, at least it was really interesting to
me to implement. The most important part of the raw post was an actual
post content. And it was returned mostly in the form of html both for
posts written via Livejournal web interface and with the client (which
was expected since client converted markdown to html before sending).

Given that what I wanted to get was to convert noncomplex html like
this:

```html
<h1>One more heading</h1>

<p>First <em>paragraph</em></p>

<blockquote>As someone said</blockquote>
```

to a pretty markdown form such as this:

    # One more heading

    First *paragraph*

    > As someone said


To complicate things a bit more Livejournal had a couple of custom
tags in use. The trickiest one was `lj-embed` that was created for any
embed from youtube or a number of other services. At first problem
looked unsolvable there, because by default response contained only
tags like `<lj-embed id="1">` which gave no chance to guess the
contents.  Luckily Livejournal once was an open souce platform and a
number of people made snapshots of the source code before it got
closed. After a thorough inspection of xmlrpc api
[implementation][ljprotocol] I found that there was a magic
undocumented flag `get_video_ids` which allowed to add a bit more
information to the `lj-embed` and turn it into something like
`<lj-embed source="youtube" vid="1bU7COLWJE0">` which was totally
enough to restore original embed code.

In order to put that together, I looked for an appropriate html
parsing library and after a couple of experiments it turned out that
[plump][plump] library was completely sufficient for the purpose. The
trick is that `plump` can not only parse html but also serialize it
back and this is where it appeared to be super easy to plug in and
turn html generation into markdown generation. Serialization is
actually done by one generic function, `plump-dom:serialize-object`
and Common Lisp generics and auxiliary methods made it trivial to
achieve desired behavior.

First, I was able to target only tags I was interested in by
specifying the type of the first argument, which represented the
element. After that, by specifying `:around` method I could completely
control printing behavior. Let's say that we want to implement only
paragraph, links conversion and `lj-embed` support. Here is how the
code would look like (you can check [full source][markdowwnify].

```common_lisp
(defmethod plump-dom:serialize-object :around ((node element))
    (cond
      ((tag-name-p node "P")
         (loop for child across (children node)
               do (serialize-object child))
         (format *stream* "~%~%"))

      ((tag-name-p node "a")
       (let ((str (string-trim '(#\Space #\Newline)
                               (with-output-to-string (out)
                                 (let ((*stream* out))
                                   (loop for child across (children node)
                                         do (serialize-object child)))))))
         (format *stream* "[~a](~a)" (if (> (length str) 0)
                                         str (attribute node "href"))
               (attribute node "href"))))

      ((tag-name-p node "lj-embed")
       (cond
         ((string= (attribute node "source") "youtube")
          (format *stream* "<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/~a\" frameborder=\"0\" allow=\"autoplay; encrypted-media\" allowfullscreen></iframe>" (attribute node "vid")))
         (t (call-next-method))))

      (t (call-next-method))
    ))
```

What happens is that we think of every paragraph as of a block of text with
two newlines after it and this is what happens. By calling
`serialize-object` we pass control back to generic and recursively it
can return back to this method to do any conversions we're interested
in for the child nodes of the paragraph.

Also, another awesome part of this api is that `serialize-object`
doesn't return a string, it prints to a stream and that means that
we don't need to waste our time with doing string concatenation and
related housekeeping but we are still in full control of the fate of
the content to be printed.

Link conversion shows it very good. I override `*stream*` stream that
`plump` uses for printing and catch all the children output into a
string, which gives me a way to produce a link like `[contents](url)`.

In this sense, `lj-embed` conversion doesn't show anything new, but I
decided to add it to show how easy it was.

Final conversion code turned out to be more or less lengthy but that's
only because I was already too tired and lazy to come up with smart
solutions and the only thing I cared about was to make enough test
cases and make them pass.

One library that I found super useful at this step was
[cl-strings][cl-strings] that provides basic string manipulation
utilities that all languages except Common Lisp provide out of the
box. Of course, I could have used `cl-ppcre` but `cl-strings` just did
the job and again made the logic trivial.

I can say that it was magically simple for me due to how beautifully
different Common Lisp played together in api. And `plump` is awesome
by itself!

### Filename

Once I had all proper fields and post source in place the last
remaining part was to actually save the file. For existing posts it's
easy, but for new ones I had to come up with a name. When I write a
post with a client, `cl-journal new post-name` command generates
filename `YYYY-MM-DD-post-name.md`. `post-name` was completely
arbitrary and for reverse action I decided to generate it from the
title. I almost decided to write such a library myself but then
stumbled upon [reddit post][reddit slugify] where author presented
[cl-slug][cl-slug] library which solved exactly this problem and not
only for English but also for a number of other languages including
Russian. Thank you André!

After slug was there I had to ensure the uniqueness of the filename.
final logic turned out to be pretty straightforward: generate base in
the form of `YYYY-MM-DD-slug` where timestamp was creation timestamp
of the post and if there was already a file with the name
`YYYY-MM-DD-slug`, keep adding `-1`, `-2` etc at the end till we found
a vacant spot.

Here is a full code of the sub:

```common_lisp
(defun generate-unique-filename (db datetime base)
  "Generate a filename that does not yet exist in the database
   based on datetime (yyyy-mm-dd hh:mm:ss) and a base (any string).

   Function will generate a base name of the form <date>-<slug>.md
   and in case such a filename already exist, will keep adding increasing
   postfix numbers till it finds a vacant spot"
  (labels ((gen-name (date slug counter)
             (if (equal counter 1)
                 (format nil "~a-~a.md" date slug)
                 (format nil "~a-~a-~a.md" date slug counter))))

    (let ((date (car (split datetime)))
          (slug (slugify base))
          (ht (to-hash-table db :key-sub #'filename)))

      (with-output-to-string (out)
        (loop with cnt = 0
              for name = (gen-name date slug (incf cnt))
              while (gethash name ht)
              finally (format out "~a" name))))))
```

Did I just use `loop` again? Oh, my. And `to-hash-table` was useful
again with a little modification of `:key-sub` parameter that allowed
to have anything from the post as a key. In this case, using
`filename` as a key made generation logic really simple.

Now I have all parts in place - all posts could be fetched from the
server, I could determine which posts were updated remotely or just
didn't exist locally and I could convert remote representation back to
the markdown text (I omitted the code that turned fields into a header
with `title:` and other fields but it's there of course.

By the way, in case usage of the format in `finally` part surprises
you, I had to add it, because otherwise, sbcl kept serializing filename
as an array of characters for some reason.

`merge-fetched-posts` function contains one of the most epic usages of
`loop` macro I ever did. Let me just paste it as an excerpt from the
sub, full code is [there][cl-journal merge]:

```common_lisp
(loop for event in (reverse (events store))
        for itemid = (getf (getf event :event) :itemid)
        for post = (gethash itemid ht)
        when (and (not (gethash itemid visited))
                (or (not target-itemid)
                    (equal itemid target-itemid))
                (or (not post)
                    (older-than-p post (getf event :sync-ts))))
        do
        (stuff))
```

All the logic fit nicely in default features of `loop`. `reverse` it's
there because I wanted to convert only the most fresh version of the
post that I downloaded, `for` part of the macro played the role of
`let` and assigned values on every loop iteration and `when` included
all the logic necessary so that loop body executed only in case it had
to: in case post was not already generated during this run, it didn't
exist in database or database version was older than remote.

One important thing in this logic was `synced-from-fetch` flag for
every post. Whenever I merge it I raised this flag to 1 and whenever I
updated or created a post with the client I reset it to `nil`. Why
important? Simply because the conversion process was and still isn't
perfect, I wanted to have a nondestructive way to run merge
functionality again and again, end this is what
`remerge-fetched-posts` function and related cli command does. With
this flag I could take all the posts that I already converted but that
were not touched after and merge them again. This gave me an
opportunity to already have markdown files of some sorts and still
work on better conversion afterward.

From potential improvements:

* `plump` doesn't handle accidental closing image tags as I wanted
  them to (I want them to be ignored)
* All paragraph contents tend to be turned into one line and in a
  perfect world I wanted my converter to also have line length
  limitation and nicely wrap the code if necessary.
* And probably many more bugs and mistakes that I did myself.

After all this functionality got implemented, the client reached near
perfect state to me, because if you remember it's unidirectional
nature was one of the client limitations and now it actually didn't
matter where a post was written or updated, fetch end merge steps could
incorporate all the changes back. And what's even cooler, now the client
can be used for any blog and from the day one it's possible to get all
posts offline and work as if you were using `cl-journal` forever.

[file-api]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/file-api.lisp#L71
[ljprotocol]: https://github.com/apparentlymart/livejournal/blob/master/cgi-bin/ljprotocol.pl
[plump]: https://shinmera.github.io/plump/
[markdownify]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/markdownify.lisp
[cl-strings]: https://github.com/diogoalexandrefranco/cl-strings
[cl-slug]: https://github.com/EuAndreh/cl-slug
[cl-journal merge]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/cl-journal.lisp#L243
[defconstant]: http://www.sbcl.org/manual/#Defining-Constants-1
[local-time]: https://common-lisp.net/project/local-time/
