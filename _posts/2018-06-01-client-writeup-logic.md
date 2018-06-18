---
layout: post
title: "Part 2: Client logic"
keywords: "common lisp, livejournal, blog, cl-journal"
category: blog
tag: building-blog-client
---

There are a lot of different bits related to the client that were
pretty fun to solve, especially in order to get git-like behavior,
but nothing of that matters if we cannot communicate with the server,
so let's start with that.

### XMLRPC

Livejournal uses XMLRPC as a protocol for communication. Documentation
is [old][xml-rpc] but serves it's purpose. However, I had to check the
last opensource version of the protocol implementation to make sense
out of it for a back sync functionality. I'll return to it later, but
now let's see how that can be done at all.

I looked for a library to use and initially my choice was
[s-xml-rpc][s-xml-rpc]. I don't remember exactly why I decided to get
rid of it. I think it was due to the fact that s-xml-rpc returns
result in term of a hierarchy of objects and it proved to be difficult
to maintain.

Instead of that, I've settled with [rpc4cl][rpc4cl] which simply
returns a nested list and that's precisely what I need. To simplify
the code I made s simple wrapper that takes adds host information to
every call:

```common_lisp
(defun rpc-call (method &rest method-parameters)
  (apply #'rpc4cl:rpc-call *service-endpoint*
         nil nil method method-parameters))
```

After that, all remote calls naturally fit into the code. Here is as
example a sub to get a so-called challenge from Livejournal service:

```common_lisp
(defun getchallenge ()
  (->
   (rpc-call "LJ.XMLRPC.getchallenge")
   (getf :challenge)))
```

Please note `->` from [cl-arrows][cl-arrows] here. I seriously cannot
imagine my lisp coding with it, because using the arrow prevents all
sorts of deep nestings and keeps code clear. Since it's not always
easy to remember in which place the previous result goes by default in
the last code I always use `-<>` instead which allows putting a
placeholder `<>` that will be replaced with the result. This way makes
argument placing more explicit. As an example `getchallenge` can be
transformed to this:

```common_lisp
(defun getchallenge ()
  (-<> "LJ.XMLRPC.getchallenge"
   (rpc-call <>)
   (getf <> :challenge)))
```

One of the distinctive features of Common Lisp is its live editing
process where one can start interpreter, load necessary code there and
do all the changes right in it compiling changed functions if
necessary without a need to compile and start the program all over again.

In case of the API, it's particularly useful. I defined three dynamic
variables - `*service-login*`, `*service-password*`,
`*service-endpoint*`.  I set them once the repl starts and after that
I can do any experimentation with the api with help of all the power
the language provides. And after I [wrapped][lj-api] all service
endpoints as functions I literally could do all blog manipulations
without leaving the editor. It proved to be so useful that I kept the
running lisp process for months without any need for a restart.

The only improvement I made recently was to add unit tests into the
toolbox and it works wonderfully with repl-driven development. Why is
it cool?  With a usual way of developing one of the hardest bits is to
recreate an environment where tests happen, and in cases where there
is no obvious way to implement something, changing implementation can
have a lot of impact on how tests are written. With Common Lisp, I can
experiment with code freely until I get something working without
overhead related to environment and create supporting architecture
only after that and I can still run all the tests right there! Change
a function and recompile only one specific test till everything works
and then compile the whole suite to see if everything's in place.

I'll talk about this bit later, it's so fascinating that I can safely
say that it's one of the features that really make me stick to the
language, I enjoy every second of it.

### Storage

I wrote the client primarily for myself and I didn't want to invest
in security more than a secure password storage that was achieved
but external tools. I wanted some storage, however.

First things first I decided to have a class that represents a posts
database, that looks like this:

```common_lisp
(defclass <db> ()
  ((posts :initarg :posts :accessor posts)
   (version :initarg :version :reader version)
   (login :initarg :login :reader login)
   (raw-text :initarg :raw-text :reader raw-text)
   (service :initarg :service :reader service)
   (service-endpoint :initarg :service-endpoint :reader service-endpoint)
   (fetch-store :reader fetch-store)
   ))
```

That's the most recent version, of course, the most minimal version
contained only `posts` slot and thanks to the lisp interactivity more
slots could be added by class recompilation and all the live instances
got new slots automatically.

Anyway, as a next step, I wanted to serialize the database and it's
contents and store it into a file. This is very Common Lisp generics
step in. They are completely orthogonal to classes and make it super
simple to do all sorts of recursive definitions for particular
functionality. In this case, I defined a generic `to-list` and wrote
it's implementation for the database:

```common_lisp
(defmethod to-list ((db <db>))
  `(:login ,(login db)
    :version 2
    :service ,(service db)
    :raw-text ,(raw-text db)
    :service-endpoint ,(service-endpoint db)
    :posts ,(mapcar #'to-list (posts db))))
```

To make this method work completely I had to only implement this
method for post `<post>` class, which I did.

```common_lisp
(defmethod to-list ((post <post>))
  (list
   :itemid (itemid post)
   :anum (anum post)
   :ditemid (ditemid post)
   :url (url post)
   :created-at (created-at post)
   :updated-at (updated-at post)
   :ignored-at (ignored-at post)
   :server-changed-at (server-changed-at post)
   :synced-from-fetch (synced-from-fetch post)
   :log-ts (log-ts post)
   :filename (filename post)
   :journal (journal post)
   ))
```

Super simple, after that saving database to a file became trivial:

```common_lisp
(defun save-posts ()
  (with-open-file (out *posts-file*
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (pprint (to-list *posts*) out))))
```

`*posts*` here is another global var that holds a reference to the
open database and *posts-file* is a relative path pointing where the
posts file should live.

What happens in the sub is that we pretty print s-expression that
happens to be a result of `to-list` method call into a stream that
happens to be an open file. `with-standard-io-syntax` macro ensures
that all the dynamic variables are reset to their default state for
the time of printing.

After this is done we can use yet another Common Lisp feature that
makes it trivial to save changes on all meaningful actions. Common
Lisp supports aspect-oriented programming in a sense of auxiliary
methods. That means that for any generic function we can hook into
any point during its call. I had generics for publishing, updating
and deleting the post and all save logic is as simple as:

```common_lisp
(defmethod publish-post :after ((db <db>) (post-file <post-file>))
  (save-posts))

(defmethod delete-post :after ((db <db>) (post <post>))
  (save-posts))

(defmethod update-post :after ((db <db>) (post <post>))
  (save-posts))
```

An obvious downside of this approach is that such an implementation is
tied to the class and not to object instance and that can lead into
all sorts of troubles characteristic to global state.

Now that the file is saved we need to be able to restore it. That's
also easy:

```common_lisp
(defun restore-posts ()
  (read-parse-file *posts-file*
                   #'(lambda (l)
                       (setf *posts* (create-db-from-list l))
                       )))
```

As you see there are no safety checks there, which might be needed
were I to worry about it. I chose for simplicity in this case since
for the time being I'm the only user of the program.

`create-db-from-list` is an ordinary function in this case, not
generic, and there is a `create-post-from-list` for posts. Now, during
the development, the structure of the database and posts inevitably
changes and we need to handle that somehow and that's why database has
`version` slot.

What I decided to do was to do database migrations right during the
read phase. In order to do that I find a unique feature for the next
version of config, do migration and run the same function recursively
and by this way eventually get a most uptodate structure that will be
nicely serialized on next save.

```common_lisp
(defun create-db-from-list (l)
  (cond
    ((null (find :version l)) (create-db-from-list (migrate-db-v0-v1 l)))
    ((null (find :service l)) (create-db-from-list (migrate-db-v1-v2 l)))
    (t (create-db-from-list-finally l))))
```

### Publishing

Now that I had a database I wanted to fill it somehow with posts! Each
post is represented by a markdown file with a header that contains
custom fields.  For example:

    title: This is a cool post
    tags: this, is
    privacy: friends

    # Intro

    This post will contain

    * A header
    * A list
    * A couple of paragraphs

Please note that till this moment we didn't reach any cli interface and
git-like state management, hence I wanted to implement a simple
function that would take a file, parse it, convert markdown into html
and publish it as a post.

Task itself is also recursive meaning that after we parse a file and
get a list of fields plus a text we need to parse the again to map all
the fields to a representation Livejournal understands.

The first part you can check out an [implementation][file-api] of
`parse-post-file` function. What it does is it reads header line by
line and treats all the first field of the form `field: value` as
individual fields and then all the rest as a markdown body of that
post which it converts to the html.

After this step is done an instance of `<post-file>` is initialized
and we can already do all sorts of things with it, not just
publishing. For example, we can check if it's a draft. But let's
publish.

For that we need to convert this object to the format `lj.postevent`
understands. For this `to-xmlrpc-struct` generic is defined, which is
super handy since I can do an implementation for both `<post>` and
`<post-file>` and use whatever is at hand. For example for a new post
it's always `<post-file>` and for updates it's `<post>`.

For `<post-file>` I decided to go with a layered approach where I have
a basic object and then enrich it with all sorts of additional helpers
that contain logic for particular fields. Here is `to-event-list`
which is used by `to-xmlrpc-struct`:

```common_lisp
(defmethod to-event-list ((post <post-file>) &optional (transform #'identity))
  (let ((l (list
            :event (if *raw-text* (body-raw post) (body post))
            :subject (title post)
            )))
    (-<> l
         (add-props (fields post))
         (add-privacy-fields (privacy post))
         (add-usejournal (journal post) <>)
         (add-date)
         (funcall transform <>))))
```

Every `add-*` function should return a new object that is potentially
the same as `l` but can be modified version of it. Here is
`add-usejournal` which is used whenever I want to post to a different
journal than my own:

```common_lisp
(defun add-usejournal (journal plist)
  (if (not (null journal))
      (concatenate 'list
                   plist
                   (list :usejournal journal))
      plist))
```

This conditional is not that elegant by itself, but the general pattern
proved to be very useful and I used it in many different places.

Once I got an even in Livejournal view of it the publish function
itself becomes really simple:

```common_lisp
(defmethod publish-post ((db <db>) (post-file <post-file>))
  (set-credentials db)
  (let ((*raw-text* (raw-text db)))
    (let ((post (create-new-post post-file)))
      (push post (posts db)))))
```

And if you remember for the previous parts database will be saved to
file whenever this method is called. And now, if we call this method
from `pre-commit` hook we will get all changes saved to disk and will
be able to add them to the index and be included in the
commit. `set-credentials` here is to ensure that we have all api
variables set before we make a call. The most interesting part of it
is `get-password` that not just retrieves the password, but it also
requests and saves it in case of absence.

And this is it about general client framework. With a list of `<post>`
objects in the database we can do all sorts of calculations and answer
to all sorts of interesting questions especially because there is a
direct connection with the corresponding file on disk that can be
converted to `<post-file`> at will.

How can I get the last published post? Easy:

```common_lisp
(defun get-last-published-post (db)
  (car (sort (posts db) #'> :key #'created-at)))
```

How can I know the title of particular `<post>`? Super easy:

```common_lisp
(defmethod title ((post <post>))
  (title (read-from-file (filename post))))
```

And that's just two examples, it's always possible to start the repl,
load a posts database from file manually and start playing with
contents.

To make repl experience even more pleasant Common Lisp provides
`print-object` generic that is responsible for the text representation
of the object. Hence we can print any information we like from the
object instance.

```common_lisp
(defmethod print-object ((post <post>) stream)
  (format stream "<post filename:~a url:~a>~%" (filename post) (url post)))
```

The last bit before implementing cli interface is a state management
and it can also be easily implemented using the same operations on
`<post>` and `<post-file>` objects.

What do I mean by state management? Looking at the blog folder we
should be able to understand which posts are new, updated deleted or
are drafts and they all are defined in different terms:

* New post is the one that exists in the folder but is not mentioned
  in the database
* Deleted post is the one that does not exist in the folder, but does
  exist in the database
* Modified post is that one that exists in both places, however, it's
  recorded timestamp is lower than last modification timestamp of the
  file on disk
* The draft is a file in the folder that has a field `draft: 1` in the
  header

What they all share is that they all have a function that is able to
generate a list of items of a specific kind and a set of messages to be
printed depending on a number of items got in the previous step. Of
course, the line can be printed differently from case to case.

For example, we can have the following output:

    There a drafts file

        2018-06-04-books29.md

    There is a new file to publish

        2018-06-06-test.md

    There are 2 modified files to update

        2009-10-12-no-title.md
        2013-12-31-eshe-odin.md

    There is a deleted file to unpublish

        2011-01-23-no-title.md

Let's take drafts as an example. Here is a function to retrieve a
list:

```common_lisp
(defun get-draft-files ()
  (->> (get-markdown-files)
       (remove-if #'(lambda (fname) (get-by-fname *posts* fname)))
       (mapcar #'read-from-file)
       (remove-if-not #'draft)
       (mapcar #'filename)
       ))
```

`cl-arrows` rocks again there. What happens is that we get a list of
all markdown files in the folder, remove any files that exist in the
database, parse the reminder, kick out all files that do not contain
the mentioned field and retrieve filenames back from `filename` slot
in resulting `<post-file>` objects. Not that performant, I know, but
it doesn't really matter on this scale and my main aim was
correctness and readability, not speed.

Once we have a list, we need to have a pretty printer for it and it
should be similar for all the cases. At this point in time, I decided
to test my defmacro-fu and came up with a macro, that looks like this:

```common_lisp
(with-files draft (get-draft-files)
  "There are ~a drafts files~%"
  "There a drafts file~%"
  "No drafts found~%")
```

What this macro does is it generates another function named using the
second argument (it'll be `with-draft-files` in this case) and that
function will print one of the strings depending on the number of
items from the second argument (`(get-draft-files)` in this case) and
print a list of items in a specified manner. The resulting
`print-status` function looks very clean after all these
manipulations:

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

I use one or another printing function depending on what is produced
but the `with-*` function, it can be a plain list of filenames or a
list of `<post>` instances or anything else and I don't need to worry
about it at the generation step and have all the flexibility to do it
at printing step, view layer is separate!

Speaking of modified posts. Whenever I publish a post I run
`get-universal-time` and record it along the filename. Then it takes a
super simple predicate to check whether a particular post is modified:

```common_lisp
(defmethod modified-p ((post <post>))
  (let ((fname (filename post)))
    (and
     (probe-file fname)
     (< (or (ignored-at post) (updated-at post) (created-at post))
        (file-write-date fname)))))
```

You see `file-write-date` function there. This logic works really well
if no one touches files, however, whenever files get moved or repo gets
cloned to another location, the timestamp from it gets distorted and all
files are marked as modified. This is precisely the reason for
`ignored-at` field there. Whenever I want to mark all the files as
uptodate I run this function:

```common_lisp
(defun ignore-all ()
  (let ((ts (get-universal-time)))
    (loop for post in (posts *posts*) do
          (setf (ignored-at post) ts))
    (save-posts)))
```

This was one of the first times I used `loop` macro and I really fell
in love with it some time after that.

### Markdown

Last fun bit from the basic implementation is about markdown
formatting.  I decided to use it from the very beginning simply because
it's an order of magnitude better to write then html and still allows
to have markup in place comparing to real plain text.

And of course, both vim and emacs have excellent markdown modes and
make it a real pleasure to write.

What could be interesting about writing markdown? Well, to extend it!

I had two extensions planned. First of all, I didn't want to use real
urls for links between the post. If I did, I would be tied to one
particular web service and migration to another one would be
error-prone. If all links in the source code point to other markdown
files, that means that whenever I decided to republish all posts
somewhere else, I could have all the links resolved.

For markdown rendering I used `cl-markdown` library and I'm not sure
if it was the best choice possible. Why? Because it's source code is
close to impenetrable to me and documentation is almost non-existent.
Maybe I'm not a good reader, but I took me many evenings to get my
head around library internals and to understand ways to extend it.

For links generation I kept the assumption that I only have one
database open at a time and it live in `*posts*` variable.
`cl-markdown` unfortunately doesn't export any methods it uses for
html generation and I had to [get][markdown] into library package to
extend it the way I want.

Inline elements are rendered using `render-span-to-html` generic
function and Common Lisp rocks again there because it allows adding
an auxiliary method for it and target a specific set of arguments, that
allows to keep method body clean from unnecessary checks:

```common_lisp
(defmethod render-span-to-html :before
    ((code (eql 'inline-link)) body encoding-method)
  (let ((record (cl-journal.db:get-by-fname cl-journal::*posts* (cadr body))))
    (if record
        (setf (cadr body) (cl-journal.db:url record)))))
```

What happens is that for links the first element in the `body` element
holds a link that will be put in `href`, and in before element we can
do a lookup in the database and replace a link if necessary. I think
it's an amazing level of flexibility with this amount of effort.

Another extension that I wanted to have was about links to the other
blogs. Livejournal has some custom tags for different purposes and in
this case it uses `<lj user="can3p">`, where user attribute holds the
name of a blog. `cl-markdown` allows extensions and in order to make
`{lj-user can3p}` render to the desired tag it's possible to use an
official extension logic:

```common_lisp
(defextension (lj-user :arguments ((name :required)) :insertp t)
  (setf name (ensure-string name))
  (let ((safe-name (html-safe-name name)))
    (ecase phase
      (:parse)
      (:render
       (format nil "<lj user='~a'>" safe-name)))))
```

This looks and is indeed easy but it took me quite some time to figure
all the details out.

This part concludes main highlights of the client. Of course, there is
more, you can check out the amount of [commands][main] supported at the
moment.  Some of them like `new` or `last` are there just for
convenience to make it easier to start writing a new post or quickly
open the last one in case of typos, some like `status` or `url` are
informative in a sense that they help to query the database and give out
information, last group like `ignore-all` is used to fix the
imperfections in file state handling logic.

There is one more group I haven't talked about, which is `fetch`,
`merge` and `remerge` and this is what I'm going to talk about next.


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
