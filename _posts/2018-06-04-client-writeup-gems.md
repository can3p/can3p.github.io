---
layout: post
title: "Part 5: Gems and pitfalls"
keywords: "common lisp, livejournal, blog, cl-journal"
category: blog
tag: building-blog-client
---

All the points below summarize the things I enjoy using in Common Lisp
and are not especially the ones preferred or recommended by
community. Also proof reading showed that many of the examples were
already mentioned earlier, but I still think it's good to have them
all in one place for the reference. Here you go.

### Generic functions

If I were to choose the single most amazing feature of the language I
would choose generic functions. Why?

The reason is that decoupling them from the object resulted in amazing
freedom and flexibility when using them.

First of all, generic function dispatch against all it's arguments and
second, all of the arguments can be of any type and not some object
hierarchy.  What the means that it's very easy to build recursive
implementations that do some preliminary work with arguments and then
call the same function again which makes generic function dispatch
differently and execute another implementation. I used this trick a
lot.

One example of such a definition could be `fetch-posts` function.
What we know that we want to fetch posts to some kind of database,
which has a store. Such a definition means that we can have objects of
both types at our disposal. With described technique it's simple to
make it work as desired:

```common_lisp
(defgeneric fetch-posts (db))

(defmethod fetch-posts ((db <db>))
  (set-credentials db)
  (fetch-posts (fetch-store db)))

(defmethod fetch-posts ((store <store>))
  (do-actual-fetch))
```

When we call `fetch-posts` against the database implementation just
extracts store from it and call the function again which execute
another method and now we can fetch posts both with store or database
instance without any effort.

Another bonus from decoupling is that you are free to create any new
function and implement it against any object hierarchy. It may be not
even a hierarchy but just a set of types.

Remember function `to-hash-table` that I used before? It is
implemented as a generic function.

```common_lisp
(defgeneric to-hash-table (source &key))
```

With such a definition you can just implement it against any type that
you want without any restrictions. When I needed it for the database I
wrote this implementation:

```common_lisp
(defmethod to-hash-table ((db <db>) &key (key-sub #'itemid))
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (post (posts db) ht)
        (setf (gethash (funcall key-sub post) ht) post))))
```

And when later it became clear that `<store>` class can also benefit
from one, I just implemented it:

```common_lisp
(defmethod to-hash-table ((store <store>) &key)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (item (events store) ht)
      (let ((itemid (-<> item
                         (getf <> :event)
                         (getf <> :itemid)))
            (ts (getf item :sync-ts)))
        (setf (gethash itemid ht) ts)))))
```

Having said that I need to mention that this freedom applies to
external functions as well and many of the external libraries expose
precisely generic functions to control their behavior. The good
example is with `plump` library when it was enough just to implement
function against the set of types without doing any work at all with
types.

Last bit that I want to praise is auxiliary methods. This is just
incredible because of flexibility it gives. When I just started coding
I wanted to have database saved on any operation. Post created? Save!
Updated?  Save! Deleted? Save!

Using `:after` modified allowed me to completely decouple the logic
and for example, the main implementation of `publish-post` knew nothing
about saving to the database, but meanwhile, in the other package it
was as easy as

```common_lisp
(defmethod publish-post :after ((db <db>) (post-file <post-file>))
  (save-posts))
```

And it's done.

Another use case can be found in markdown handling that I did. What I
wanted to do was to write a file with links pointing to the local files
and translate them to the real urls on the markdown compilation phase.

```common_lisp
(defmethod render-span-to-html :before
    ((code (eql 'inline-link)) body encoding-method)
  (let ((record (cl-journal.db:get-by-fname cl-journal::*posts* (cadr body))))
    (if record
        (setf (cadr body) (cl-journal.db:url record)))))
```

`:before` method does not in general change the behavior of the main
code, however, it has access to all it's arguments and in this case I
check if whatever is passed as an url can be resolved to a post url
from the database and modify argument accordingly. After that, I don't
really need to touch library logic, it works as usual.

### Streams

Streams are really powerful. Languages that don't have them (let's say
javascript) often end up with ugly hacks or additional code or with
string concatenation all over the place and with streams all the api
gets streamlined immediately and the neat thing is that it's still
totally under control.

I benefited from this fact quite a lot. One example is saving data to
the file. Common Lisp provides a function `pprint` that prints data
structure in a nice way. Default is stdout, but stream can also be
passed as an argument. Given that saving state to file becomes as
simple as:

```common_lisp
(defun save-posts ()
  (with-open-file (out *posts-file*
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (pprint (to-list *posts*) out))))
```

And what's cool is that whatever is printed with `pprint` by the
definition can be consumed by `read`. Since `read` invokes Common Lisp
reader, it has some security implications for sure. In my use case
however, I didn't really bother about this part, because I'm a sole
user of the `cl-journal` and database reading and writing logic is
written in such a way, that I can change it to whatever format anytime
without any friction.

Another usage was showcased in part about html to markdown conversion.
Serialization logic in `plump` simply prints results in `*stream*`
variable that by default happens to be equal to stdout (if I'm not
mistaken) and in the method I could capture any part of output simply
by temporary setting `*stream*` to the other stream and then doing
whatever I found fit with the output.

```common_lisp
(with-output-to-string (out)
  (let ((*stream* out))
    (loop for child across (children node)
          do (serialize-object child))))
```

`format` works with streams too, I'll talk about it later.

### Special variables

Special variables is yet another super powerful concept. What's
special about them is that they use dynamic binding instead of a
lexical one and that means the changing the value of such not only for
the current scope but for all the code that is called from there.

This gives a powerful weapon to penetrate through layers of
abstractions without a cost of passing this variable through or making
it global in the true sense of this word.

In case of `plump` special variable provided an easy way to control
the output.

### Loop

If were to write this post a couple of months ago I wouldn't mention
loop at all. I came to Common Lisp from clojure and my brain was
really fixed on immutable data structures, hence I tried to avoid any
imperative constructs. Another moment was that loop had a
controversial perception across community some of which preferred
`iterate` and the rest tried to avoid both.

The turning point was when I started writing fetch and sync logic and
I had to iterate there a lot in many different ways. I tried loop once
then twice and then I ended up using it all over the place. Why so?

Simply because loop unify all different looping constructs that other
languages have like `for` or `while` or even `map` in one unified call
and in addition to that gives local bindings all an easy way to
execute body only under certain conditions and return from any point,
and I'm sure there are lots of things I'm not aware of.

Let me show a couple of especially impressive examples from the code.
Here is a function that prints a list of files that need to be merged:

```common_lisp
(defun get-merge-candidates (db)
  (let ((store (restore-source-posts (fetch-store db)))
        (ht (to-hash-table *posts*))
        (visited (make-hash-table)))
    (loop for event in (events store)
          for itemid = (getf (getf event :event) :itemid)
          for post = (gethash itemid ht)
          when (and (not (gethash itemid visited))
                    (or (not post)
                        (older-than-p post (getf event :sync-ts))))
            collect
            (progn
              (setf (gethash itemid visited) t)
              (if (null post)
                  (format nil "~a - ~a"
                          itemid
                          (getf (getf event :event) :url))
                  (format nil "~a - ~a (~a)"
                          itemid
                          (getf (getf event :event) :url)
                          (filename post))
                  )))))
```

Iteration goes over `(events store)`, two other `for`s work just like
local bindings. `when` part uses local bindings as well as bindings
from function scope to understand if this particular post needs to be
merged. If this check succeeds `collect` executes next form and
appends it to a resulting list to be returned. In this body we update
a list of visited posts so that any duplicate post is ignored.

Next example is silly but show loop can serve as a loop construct.

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

`with` serves as a local binding there and whenever we want to finish
we can simply call return and it's argument will be the return value.

Yet another loop combo allows an easy traversal of plist with
destructuring of key/value pair:

```common_lisp
(loop for (key value . rest)
        on (getf post-file :fields)
      by #'cddr
      do
         (format out "~a: ~a~%"
                 (string-downcase (symbol-name key))
                 value))
```

From examples above you can already spot the greatest weakness of this
macro - it's syntax is so diverse that using it looks natural only
when you read it, writing tends to be more in the field of trial and
error.

### Macros

There is a lot written about macros and their pros and cons. Main
drawback for me is that their usage or, better, usage of nonstandard
ones has a huge impact in readability of the code simply because you
need to go and understand them first and macros are not the easiest
thing to understand, especially for people without years of full time
lisp development like me.

From the other side writing them is a total pleasure because you can
watch the language mold under your hands. I'll show one example there
and it's naming is probably totally incorrect, but it solved the issue
I had.

To print the status I had to print information about different
states - print a list of new files, drafts, updated files etc. Each of
them had it's own sub to produce a list and it's own text of
course. To make it more human-friendly I wanted to have not one but
three text - for zero, one and many results.

I wrote initial logic as a function and amount of duplication became
obvious very soon. I decided to give macros ago and imagined a perfect
way to generate a status of a certain kind. I could do a macro that
does code execution in place but that would sit in the body of a
single function inflate it's size. Based on that I decided to make a
macro to generate a function definition.

This was a perfect syntax in my opinion:

```common_lisp
(with-files new (get-new-files)
  "There are ~a new files to publish~%"
  "There is a new file to publish~%"
  "No new files to publish~%")
```

That would generate a function with the name `with-new-files` that
takes a callback to print a list that's a result of calling
`(get-new-files)` form and prints header by itself and items list with
this callback.

The callback was an extension point to remove any constraint on the
type of list items and let actual code deal with it.

With the macro implemented actual status code again became really
trivial.

```common_lisp
(defun print-status ()
  (flet ((print-names (items)
           (format t "~%~{    ~a~^~%~}~%~%" (mapcar #'filename items)))
         (print-string-names (items)
           (format t "~%~{    ~a~^~%~}~%~%" items))
         )
    (with-draft-files #'print-string-names)
    (with-new-files #'print-names)
    (with-modified-files #'print-names)
    (with-deleted-files #'print-names)
    (with-fetched-files #'print-string-names)))
```

And here is the actual macro code:

```common_lisp
(defmacro with-files (name accessor multiplemsg singlemsg nomsg)
  (let ((fn-name (intern
                  (concatenate 'string
                               "WITH-"
                               (symbol-name name)
                               "-FILES"))))
    `(defun ,fn-name (cb)
       (let ((items ,accessor))
         (if (> (length items) 0)
             (progn
               (if (> (length items) 1)
                   (format t ,multiplemsg (length items))
                   (format t ,singlemsg))
               (funcall cb items))
             (format t ,nomsg))))))
```

What happens there is I'm assembling function name and then return
function definition with placeholders replaced with the data from
arguments.

Macro is not a tool for any task but it can be really life changing if
you only ever programmed with macroless languages.

### Format

I find `format` really fascinating. Most of the languages mimic `C`,
they implement `sprintf` style functions and it's a shame given how
much more powerful `format` is.

First of all, it works on streams, that gives a lot of power on their
own, as I wrote earlier. Next it literary has all functionality
necessary to always print to output with one call.

It's possible to print a quoted value, a value without surrounding
quotes, add all sorts of paddings to the printed data and to even
print arrays!

Here are few spells:

```common_lisp
(format nil "~{~a/~}~a-~2,'0d-~2,'0d-~a.md"
        rest
        (getf date :year)
        (getf date :mon)
        (getf date :day)
        name
        )
```

What happens there? We print a pathname. `rest` contains a list with
folders, date parts go after that and in the end we want to print the
name. `~{~a/~}` prints folders separated by the forward slash, `~2,'0d`
ensure that two digit number is printed and pads it with leading zeros
if necessary. If arguments are `((list "path" "to" "file") 2018 3 4
title` result will be `path/to/file/2018-03-04-tile.md`

Or here is how I print a list of files in status, `~%` means newline:

```common_lisp
(format t "~%~{    ~a~^~%~}~%~%" items)
```

Format can do much more and it simply eliminates manual fiddling with
data to prepare it for printing.

### Little things

There are lots of decisions in Common Lisp standard package that make
you only wonder why didn't the find the way to any other language.

For example, `read-line` function accepts an argument that will be
returned if the end of stream was reached. Why would you need this?
Simply because returning custom value there can make some upper-level
logic more generic.  Btw, `dolist` does that too.

Another small nicety is that many functions on collections accept
parameters like `:test` or `:key` that immediately make them more
useful.

Here is how last published post is found, for example:

```common_lisp
(defun get-last-published-post (db)
  (car (sort (posts db) #'> :key #'created-at)))
```

Not effective, I know. Or `member` function in this example:

```common_lisp
(defun known-editor (e)
  (member e '("vim" "emacs" "emacsclient") :test 'string=))
```

It's string there, but having this parameter immediately allows to
have list members of any type as long as there is an equality check
for them.

## Common Lisp pitfalls

### Packages

Before writing `cl-journal` I didn't have too much experience working
with Common Lisp, so I decided to pick up whatever I considered to be
latest best practices and try to live with it. I've split all the
functionality into separate packages and tried to export and import
only really necessary functionality.

Soon I've found out that this way of development was much more verbose
there than in other languages and the reason was mostly CLOS. Packages
serve as containers for symbols in Common Lisp and any symbol you want
to share between packages needs to be exported.

That means that class name should be exported as well as all generic
functions generated for it accessors. A good example is
[cl-journal.db][db] package. Three classes with a handful of slots
each generated a long list of symbols to export and besides that, there
were still ordinary functions and special variables. And since generic
functions where magically generated it still left open questions about
how they will work if such generic functions were imported from two
different packages into third one. I'm sure this behavior is defined
somewhere, but I don't know.

In the end, I got so tired of all this maintenance that I started
importing whole packages with `:use` even though I treated that earlier
as a non recommended way.

### Standard library

I think a very thick book can be compiled from all the complaints
regarding the standard package of the language. Sometimes there are
functions that are of no interest to most people and there are many
cases when obviously necessary functions are not there.

Good examples are user input, string manipulations or external
processes.  Some things like prompt are more or less easy to do, but
password input proved to be a really painful
exercise. `uiop/run-program` is also far from the easiest function to
use. Ok, maybe I missed a very good tutorial on this one, but I had to
go through it's code several times to understand the details or
meaning of it's parameters.

Common Lisp comes from the time when it was if not mainstream but a
widespread language with lisp machine legacy and from what I
understand this had an influence on it's relations with outside world
and that really hurts, especially comparing string and io operations
with languages like perl that do that this bit particularly well.

### Third-party libraries

That's another very common complaint. Common Lisp libraries are often
of fantastic quality feature-wise but it doesn't really help if they
have no documentation or a brief one that explains 10% of the
functionality.  And you can almost forget about library specific
tutorials. Getting over this was a rewarding intellectual achievement
for me but the price was time, lots of time.

Here I'd like to admit that `plump` library had one of the best
documentations and even a couple of projects using it in neighbor
repos.  This bit really helped me in understanding the library and
coming up with a proper solution for html conversion.

## Final thoughts

You probably spotted already that I've been mentioning Livejournal
everywhere it can give an impression that there is no way in life for
the client to support any other service. That's actually not true, but
will require a good amount of work of course. The first step would be
to abstract away remote api and the second one will be to make
`<post>` and `<post-file>` classes service agnostic. It'll be even
easier if we do not set the aim to support the same feature set for
every single platform and provide a cl-journal as a platform that can
give the same experience as it does now for Livejournal with service
specific changes.

In terms of featureset, a lot of things can be done better of course.
For example, I can think of template posts, or client can support
failures much nicer, but after using it for more than two years I can
say that none of it is something that turns it into something
unusable.

Was it a correct choice to use Common Lisp for implementation? For me,
absolutely yes, because a lot of things I implemented would have taken
twice as code to get them working and interactive development mad me
so performant that I was able to add significant features to the code
even with a very limited time I had.

One good learning for me was to add test into interactive development
workflow. It was really not that obvious for me, but comment driven
development as I did for `syncitems` with an addition of test allowed
me to write functional code even in the time of heavy sleep
deprivation that I tend to fall into.

Another good learning that I had was to follow a bottom-up approach, you
could see an example in the merge description. Whenever I approached
big task I started asking myself very simple question starting from
"How do I get the changes?" or "How do I get the filename" and solving
them slowly one by one. After they were all complete the final task
that was thought to be complex and indeed was turned out to be a very
small function with simple logic.

Thank you for reading.

[db]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/db.lisp
