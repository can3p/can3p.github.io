---
layout: post
title: "On writing a blog client in Common Lisp"
keywords: "common lisp, livejournal, blog, cl-journal"
category: blog
---

I love markdown and vim. I do blogging and I am fed up with interfaces.

## Intro

**Disclaimer** Please treat this post as an experience report more than
anything else because this is what it is. As a consequence there can be a lot
of things real lisp wizards will raise an eyebrow on.

Below you will read a story about writing a blog client for livejournal.com
webservice in Common Lisp. As one may note, it sounds like using a very niche
language for a near dead web service. To make you a bit more surprised I can
add that I've been working on it on and off for a couple of years now.

I've been using Livejournal since 2005 and never had a reason to switch to
anything else. I almost never write on political topics and don't really care
where it's hosted. I don't even need many connections but do value some and the
fact that livejournal provides near zero way to discover other blogs sometimes
works more as a benefit than a drawback, because with current state of things
when majority of users migrated to Facebook or Instagram, there is not that
much to read there. It's not engaging as Facebook, and that's *good*. The other
benefit of the service is that it's a very good site to write long posts like
this and people actually do that and there is a ton of beautiful content there,
if you're lucky to find it of course.

My main problem with livejournal always was that I didn't really own the
content in sense of storage. Blog is mine, ok, but if webservice goes down or
account gets blocked I'll lose all my ~1400 posts written to date without any
good way to get them.

From time to time I tried to use other blog platforms or static site generators
(like this site) and my real dream was to combine them and have sources in
markdown locally, so that I can edit them in my favorite editor and still
publish them on a platform that has at least some social flavor. Blog on github +
disqus is not social for sure.

I had this idea for some time and somewhere around 2016 got really burned out
by trying to ship yet another webservice in my spare time in addition to those
that I ship for my company. To recover I decided to go in the direction that
absolutely excluded any interest except my own and didn't have any money or
fame in sight.  You see now, Common Lisp and idea about better blogging at the
time where blogging is already out of fashion is almost a perfect match! And
this is where it all started.

## Idea

What does perfect blogging experience look like? It's when you write the text
in your favorite editor, save it, push it to git and it's published. And it's
when you can just grep your posts to remember something from the past or when
you can do buk updates to all the posts with system tools that you're used
too and get all changes published at once.

This was my thought process and this is what became a base of requirements.
Client should be able to:

* See new markdown files to publish
* See modified markdown files to submit changes
* See deleted markdown files and be able to delete them in the service
* Bonus points if it all happens automagically on git commit/push

That's a simple version of the client that implies that a folder with markdown
files acts as a master database that gets replicated to the slave which in
this case is webservice. An obvious drawback in this case is that I lose any
other ways to update content, be it via service web interface or it's mobile
app. So even more perfect client should be able to:

* Fetch any updates from the service
* Transform them into markdown format so that all synced posts can be later
  edited in a usual manner.

One final thought was that since all editing happens in an ordinary editor,
an obvious way to implement client is to make a cli tool for that. And if you
check requirement you'll spot that they resemble typical git operations a lot
and it makes sense to mimic git when it makes sense.

## Common Lisp nontrivialities

When you start working on a really big task, the only thing that can be said
for sure is that there are a lot of unknowns. And if in a familar language
most of the unknowns lie in the business domain, in my case a pile of unknowns
also waited to found on the language and ecosystem side. Some of the things
that I though to be trivial and that were doable in many languages appeared
to be not trivial at all in common lisp and that required a lot of time to
grasp to get a full picture. Many of the libraries I used lacked good or
any documentation and looked not maintained, however on a positive side
most of them worked really well in the end.

Let me go through the topics that appeared to be nontrivial.


### Current working directory

This one such cases. It's just one parameter, what could go wrong? Well,
it's two, at least in sbcl. It appears that there is a distinction between
cwd of external commands run by `uiop/run-program` and internal lisp
facilities. to the the feeling make a following experiment:

~~~bash
# preparation
$ mkdir -p ~/test_cwd/second_folder && cd ~/test_cwd
$ touch test.file
$ touch second_folder/nested.file
$ sbcl
~~~

and lisp session:

~~~lisp
* (ql:quickload :uiop)
To load "uiop":
  Load 1 ASDF system:
    uiop
; Loading "uiop"

(:UIOP)

* (probe-file "test.file")

#P"/home/can3p/test_cwd/test.file"

* (uiop/run-program:run-program  "ls"
    :input :interactive
    :output :string))

"second_folder
test.file
"
NIL
0
~~~

So far so good!

~~~lisp
* (uiop/os::chdir "second_folder")

0
* (uiop/run-program:run-program  "ls" :input :interactive :output :string)

"nested.file
"
NIL
0
* (probe-file "nested.file")

NIL
~~~

Oops. Yes, `probe-file` expects something else.

~~~lisp
* (setf *default-pathname-defaults* #P"second_folder")

#P"second_folder"
*  (probe-file "nested.file")

#P"/home/can3p/test_cwd/second_folder/nested.file"
~~~

And to double check.

~~~lisp
* (setf *default-pathname-defaults* #P"/home/can3p/test_cwd/")

#P"/home/can3p/test_cwd/"
*  (probe-file "test.file")

#P"/home/can3p/test_cwd/test.file"
*  (uiop/run-program:run-program  "ls" :input :interactive :output :string)

"nested.file
"
NIL
0
~~~

And that trailing forward slash in path name is important, don't lose it.
As you see, `*default-pathname-defaults*` and  `(uiop/os::chdir "second_folder")` are independent and influence different things and to keep it consistent
for both standard library and external calls it's necessary to call them
both.

It's worth saying that this distinction is of particular importance for
repl based development, because when image starts these two values are
most probably aligned.

Since I mentioned `uiop/run-program`, let's talk about it.

### User input

User input is hard in sense that it's not only about common lisp versus
the world it's also about different operating systems, and it'll be nice
if all user input works in emacs repl too.

The simpliest ever way exists for yes/no questions and it's as simple as

~~~lisp
(when (y-or-n-p "Do the thing?") (do-the-thing))
~~~

But let's say that you want to read an actual input. There is a `read-line`
function that can help. One complication is a prompt message that you may
want do display. Simply formatting it to `STDOUT` will result in bad
results in emacs in sense that prompt text will be shown after actual
input. To solve this  I found that it's necessary to force output on a
used stream and now prompt function looks like this for me:

~~~lisp
(defun prompt-read (x)
  (format *query-io* "~a: " x)
  (force-output *query-io*)
  (read-line *query-io*))
~~~

And that's not all, because there is another possible user input which is
passwords. Why is it unique? You probably will not want to enter it
clear text. Unfortunately I didn't find a proper way of doing this
magic inside of lisp and in the end a solution was to use `stty`
to manipulate input visibility and `run-program` and internal bash `read`
function to get user input:

~~~lisp
(defun prompt-read-password (x)
  (format *query-io* "~a: " x)
  (force-output *query-io*)
  (let ((password (run-program "stty -echo; read val; stty echo; echo $val" :output :string :input :interactive)))
    (string-trim '(#\Newline #\Space) password)
    ))
~~~

### Storing passwords

Next two are not common lisp specific but might be of interest for curious.
Since I read the password I wanted to store it to avoid asking again and
again. However I wasn't fond of storing it in clear text and decided to
explore sytem specific solutions

Mac os is perfect in this case because of builtin program `security` that
allows to store and retreive passwords. For linux (ubuntu in particular)
there are no builtins, however after some search I found `libsecret-tools`
that help to connect to Gnome keychain. Here is how full password management
commands look like:

~~~lisp
(defun get-password-cmd (login url)
  #-LINUX
  (format nil "security find-internet-password -a ~a -s ~a -w" login url)
  #+LINUX
  (format nil "secret-tool lookup '~a:login' ~a" url login))

#-LINUX
(defun set-password-cmd (login url password)
  (format nil "security add-internet-password -a ~a -s ~a -w '~A'" login url password))

#+LINUX
(defun set-password-cmd (login url)
  (format nil "secret-tool store --label='~a' '~a:login' '~a'" url url login))
~~~

Why is `set-password-cwd` signature different? `secret-tool` will read the
password for you and `security` expects it as an input.

### Packaging and running

Since client is a cli tool, it's really desired to compile the client system
somehow. It's not difficult by itself but I decided to place an additional
restriction there and make it easy to run and install it both in dev
environment and as an easy installable package.

A task of running the sytem as a script is perfectly solved by `roswell`
[script][roswell script] and compilation is done with a
[buildapp][buildapp script]. Since script cannot be used to build a system
and roswell and buildapp pass slightly different parameters to the entry
point I ended up moving entry point logic into a [separate package][main package].

Buildapp itself works awesome in case quicklisp specifically and lisp is
set up on the computer, however I wanted to make it really easy installable
and putting a binary file on github release doesn't sound right then. The
usual way to install a program on MacOs nowadays is Homebrew. A specific requirement
for homebrew formulas is not to use any kind of additional package manager
to build a program, hence quicklisp could not be used as is and I looked for
a workaround.

Result was [cl-brewer][cl-brewer] system that generates a Homebrew formula
with urls pointing to all the dependencies so that formula can download them
all and then use quicklisp and buildapp to make a binary without any additional
downloads.

### Editing

One last bit of user input! What I wanted to do is to allow to invoke
a user editor (set by `$EDITOR` environment variable in case it's
necessary from the cli tool. That also apppeared to be not so trivial,
because actual implementation differs by common lisp implementation
used.

Fortunately, I found [magic-ed][magic-ed] on gitub and it magically
did the right thing. Quicklisp didn't have it and I ended up including
it in my code to simplify build process

### Pre-commit hook

This one is not really common lisp specific, but it turned out to be pretty
useful. What I really wanted to do is to use the client with git to
have all my posts under version control. In this case the best ever
flow looks like this:

~~~bash
$ cl-journal new post-slug
$ # thanks to magic-ed editor opens and you can write your beautiful post
$ git add . && git commit -m "another post"
~~~

After that new post file along with all the generated metadata should be
included in the commit. A trick there is to include all changes in the
pre-commit hook. Aparently git allows that. Here is how current cl-journal
pre-commit hook looks like:

~~~bash
#!/usr/bin/env bash
cl-journal push
git add posts.lisp
~~~

## Client logic

There are a lot of different bits related to the client that were pretty
fun to solve, especially in order to get git-like behaviour, but nothing
of that matters if we cannot communicate with the server, so let's start
with that.

### XMLRPC

Livejournal uses XMLRPC as a protocol for communication. Documentation is
[old][xml-rpc] but serves it's purpose. However I had to check the last
opensource version of the protocol implementation to make sense out of
it for a back sync functionality. I'll return to it later, but now let's
see how that can be done at all.

I looked for a library to use and initially my choise was
[s-xml-rpc][s-xml-rpc]. I don't remember exactly why I decided to get
rid of it. I think it was due to the fact that s-xml-rpc returns result
in term of hierarchy of objects and it proved to be difficul to maintain.

Instead of that I've settled with [rpc4cl][rpc4cl] which simply returns
a nested list and that's precisely what I need. To simplify the code
I made s simple wrapper that takes adds host information to every call:

~~~lisp
(defun rpc-call (method &rest method-parameters)
  (apply #'rpc4cl:rpc-call *service-endpoint*
         nil nil method method-parameters))
~~~

After that all remote calls naturally fit into the code. Here is as
example a sub to get a so colled challenge from livejournal service:

~~~lisp
(defun getchallenge ()
  (->
   (rpc-call "LJ.XMLRPC.getchallenge")
   (getf :challenge)))
~~~

Please note `->` from [cl-arrows][cl-arrows] here. I seriously cannot
imagine my lisp coding with it, because using the arrow prevents all
sorts of deep nestings and keeps code clear. Since it's not always easy
to remember in which place previous result goes by default in the last
code I always use `-<>` instead which allows to put a placeholder `<>`
that will be replace with result. This way makes argument placing more
explicit. As an example `getchallenge` can be transformed to this:

~~~lisp
(defun getchallenge ()
  (-<> "LJ.XMLRPC.getchallenge"
   (rpc-call <>)
   (getf <> :challenge)))
~~~

One of the distinctive features of Common Lisp is it's live editing
process where one can start interpreter, load necessary code there
and do all the changes right in it compiling changed functions if necessary
without a need to compile and start program all over again.

In case of API it's particularly useful. I defined three dynamic
variables - `*service-login*`, `*service-password*`, `*service-endpoint*`.
I set them once the repl starts and after that I can do any experimentation
with the api with help of all the power the language provides. And after
I [wrapped][lj-api] all service endpoints as functions I literally could
do all blog manipulations without leaving the editor. It proved
to be so useful that I kept the running lisp process for months without
any need for restart.

The only improvement I made recently was to add unit tests into the
toolbox and it works wonderfully with repl-driven development. Why is
it cool?  With usual way of developing one of the hardest bits is to
recreate an environment where tests happen, and in cases where there
is no obvious way to implement something, changing implementation can
have a lot of impact on how test are written. With Common Lisp I can
experiment with code freely until I get something working without
overhead related to environment and create supporting architecture
only after that and I ca still run all the tests right there! Change a
function and recompile only one specific test till everything works
and then compile the whole suite to see if everything's in place.

I'll take about this bit more later, it's so fascinating that I can
safely say that it's one of the features that really make me stick
to the language, I enjoy every second of it.

### Storage

I wrote the client primarily for myself and I didn't want to invest
into security more than a secure password storage that was achieved
but external tools. I wanted some storage however.

First things first I decided to have a class that represents a posts
database, that looks like this:

~~~lisp
(defclass <db> ()
  ((posts :initarg :posts :accessor posts)
   (version :initarg :version :reader version)
   (login :initarg :login :reader login)
   (raw-text :initarg :raw-text :reader raw-text)
   (service :initarg :service :reader service)
   (service-endpoint :initarg :service-endpoint :reader service-endpoint)
   (fetch-store :reader fetch-store)
   ))
~~~

That's the most recent version of course, the most minimal version contained
only `posts` slot and thanks to the lisp interactivity more slots could be
added by class recompilation and all the live instances got new slots
automatically.

Anyway, as a next step I wanted to serialize the database and it's contents
and store it into file. This is very common lisp generics step in. They
are completely orthogonal to classes and make it super simple to do all
sorts of recursive definitions for particular functionality. In this
case I defined a generic `to-list` and wrote it's implementation for
the database:

~~~lisp
(defmethod to-list ((db <db>))
  `(:login ,(login db)
    :version 2
    :service ,(service db)
    :raw-text ,(raw-text db)
    :service-endpoint ,(service-endpoint db)
    :posts ,(mapcar #'to-list (posts db))))
~~~

To make this method work completely I had to only implement this method
for post `<post>` class, which I did.

~~~lisp
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
~~~

Super simple, after that saving database to a file became trivial:

~~~lisp
(defun save-posts ()
  (with-open-file (out *posts-file*
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (pprint (to-list *posts*) out))))
~~~

`*posts*` here is an another global var that holds a reference to the
open database and *posts-file* is a relative path pointing where posts
file should live.

What happens in the sub is that we prettry print s-expression that happens
to be a result of `to-list` method call into a stream that happens to be
an open file. `with-standard-io-syntax` macro ensures that all the dynamic
variables are reset to their default state for the time of printing.

After this is done we can use yet another common lisp feature that
makes it trivial to save changes on all meaningful actions. Common lisp
supports aspect oriented programming in sense of auxilary methods. That
means that for any generic function we can hook in to any point during
it's call. I had generics for publishing, updating and deleting the post
and all save logic is as simple as:

~~~lisp
(defmethod publish-post :after ((db <db>) (post-file <post-file>))
  (save-posts))

(defmethod delete-post :after ((db <db>) (post <post>))
  (save-posts))

(defmethod update-post :after ((db <db>) (post <post>))
  (save-posts))
~~~

An obvious downside of this approach is that such an implementation
is tied to the class and not to object instance and that can lead into
all sorts of troubles characterisitc to global state.

Now that the file is saved we need to be able to restore it. That's
also easy:

~~~lisp
(defun restore-posts ()
  (read-parse-file *posts-file*
                   #'(lambda (l)
                       (setf *posts* (create-db-from-list l))
                       )))
~~~

As you see there are no safety checks there, which might be needed
were I to worry about it. I chose for simplicity in this case since for
the time being I'm the only user of the program.

`create-db-from-list` is an ordinary function in this case, not generic,
and there is a `create-post-from-list` for posts. Now, during the
development the structure of the database and posts inevitably changes
and we need to handle that somehow and that's why database has `version` slot.

What I decided to do was to do database migrations right during the
read phase. In order to do that I find a unique feature for the next
version of config, do migration and run the same function recursively and
by this way eventually get a most uptodate structure that will be nicely
serialized on next save.

~~~lisp
(defun create-db-from-list (l)
  (cond
    ((null (find :version l)) (create-db-from-list (migrate-db-v0-v1 l)))
    ((null (find :service l)) (create-db-from-list (migrate-db-v1-v2 l)))
    (t (create-db-from-list-finally l))))
~~~

### Publishing

Now that I had a database I wanted to fill it somehow with posts! Each post
is represented by a markdown file with a header that contains custom fields.
For example:

~~~markdown
title: This is a cool post
tags: this, is
privacy: friends

# Intro

This post will contain

* A header
* A list
* A couple of paragraphs
~~~

Please not that till this moment we didn't reach any cli interface and
git-like state management, hence I wanted to implement a simple function
that would take a file, parse it, convert markdown into html and publish
it as a post.

Task itself is also recursive meaning that after we parse a file and
get a list of fields plus a text we need to parse the again to map
all the fields to a representation livejournal undderstands.

The first part you can checkout an [implementation][file-api] of
`parse-post-file` function. What it does is it reads header line by
line and treats all the first field of the form `field: value` as
individual fields and then all the rest as a markdown body of
that post which it converts to the html.

After this step is done an instance of `<post-file>` is initialized and
we can already do all sorts of things with it, not just publishing. For
example we can check if it's a draft. But let's publish.

For that we need co convert this object to the format `lj.postevent`
understands. For this `to-xmlrpc-struct` generic is defined, which is super
handy since I can do an implementation for both `<post>` and `<post-file>`
and use whatever is at hand. For example for a new post it's always
`<post-file>` and for updates it's `<post>`.

For `<post-file>` I decided to go with a layered approach where I have
a basic object and then enrich it with all sorts of additional helpers
that contain logic for a particular fields. Here is `to-event-list`
which is used by `to-xmlrpc-struct`:

~~~
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
~~~

Every `add-*` function should return a new object that is porentially the
same as `l` but can be modified version of it. Here is `add-userjournal`
which is used whenever I want to post to a different journal than my
own:

~~~lisp
(defun add-usejournal (journal plist)
  (if (not (null journal))
      (concatenate 'list
                   plist
                   (list :usejournal journal))
      plist))
~~~

This conditional is not that elgan by itself, but the general pattern
proved to be very useful and I used in in many different places.

Once I got an even in livejournal view of it the publish function itself
becomes really simple:

~~~lisp
(defmethod publish-post ((db <db>) (post-file <post-file>))
  (set-credentials db)
  (let ((*raw-text* (raw-text db)))
    (let ((post (create-new-post post-file)))
      (push post (posts db)))))
~~~

And if you remember for the previous parts database will be saved to file
whenever this method is called. And now, if we call this method from
`pre-commit` hook we will get all changes saved to disk and will be able
to add them to the index and be included in the commit. `set-credentials`
here is to ensure that we have all api variables set before we make a
call. The most interesting part of it is `get-password` that not just
retrieves the password, but it also requests and saves it in case of
absence.

And this is it about general client framework. With a list of `<post>`
objects in the database we can do all sorts of calculations and answer
to all sorts of interesting questions especially because there is a
direct connection with corresponding file on disk that can be converted
to `<post-file`> at will.

How can I get the last published post? Easy:

~~~lisp
(defun get-last-published-post (db)
  (car (sort (posts db) #'> :key #'created-at)))
~~~

How can I know the title of particular `<post>`? Super easy:

~~~lisp
(defmethod title ((post <post>))
  (title (read-from-file (filename post))))
~~~

And that's just two examples, it's always possible to start the repl,
load a posts database from file manually and start playing with contents.

To make repl experience even more pleasant common lisp provides `print-object`
generic that is responsible for the text representation of the object. Hence
we can print any information we like from the object instance.

~~~lisp
(defmethod print-object ((post <post>) stream)
  (format stream "<post filename:~a url:~a>~%" (filename post) (url post)))
~~~

The last bit before implementing cli interface is a state management and
it can also be easily implemented using the same operations on `<post>`
and `<post-file>` objects.

What do I mean by state management? Looking at the blog folder we should
be able to understand which posts are new, updated deleted or are drafts
and they all are defined in different terms:

* New post is the one that exists in the folder but is not mentioned in
  the database
* Deleted post is the one that does not exist in folder, but does exist
  in database
* Modified post is that one that exists in both places, however it's
  recorded timestamp is lower than last modification timestamp of file
  on the disk
* The draft is a file in folder that has a field `draft: 1` in the header

What they all share is that they all have a function that is able to
generate a list of items of specific kind and a set of messages to be
printed depending on a number of items got in the previous step. Of
course line can be printed different from case to case.

For example, we can have a following output:

~~~
There a drafts file

    2018-06-04-books29.md

There is a new file to publish

    2018-06-06-test.md

There are 2 modified files to update

    2009-10-12-no-title.md
    2013-12-31-eshe-odin.md

There is a deleted file to unpublish

    2011-01-23-no-title.md
~~~

Let's taks drafts as an example. Here is a function to retrieve a list:

~~~lisp
(defun get-draft-files ()
  (->> (get-markdown-files)
       (remove-if #'(lambda (fname) (get-by-fname *posts* fname)))
       (mapcar #'read-from-file)
       (remove-if-not #'draft)
       (mapcar #'filename)
       ))
~~~

`cl-arrows` rocks again there. What happens is that we get a list of all
markdown files in the folder, remove any files that exist in database,
parse the reminder, kick out all files that do not contain the mentioned
field and retrieve filenames back from `filename` slot in resulting
`<post-file>` objects. Not that performant, I know, but it doesn't really
matter on a this scale and my main aim was correctness and readability,
not speed.

Once we have a list, we need to have a pretty printer for it and it should
be similar for all the cases. At this point in time I decided to test
my defmacro-fu and came up with a macro, that looks like this:

~~~lisp
(with-files draft (get-draft-files)
  "There are ~a drafts files~%"
  "There a drafts file~%"
  "No drafts found~%")
~~~

What this macro does is it generates another function named using
second argument (it'll be `with-draft-files` in this case) and that
function will print one of the strings depending on the number of items
from the second argument (`(get-draft-files)` in this case) and print
a list of items in a specified manner. The resulting `print-status`
function looks very clean after all these manipulations:

~~~lisp
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
~~~

I use one or another printing funciton depending on what is produced but
the `with-*` function, it can be a plain list of filenames or a list
of `<post>` instances or anything else and I don't need to worry about
it at the generation step and have all the flexibility to do it
at printing step, view layer is separate!

Speaking of modified posts. Whenever I publish a post I run
`get-universal-time` and record it along the filename. Then it takes
a super simple predicate to check whether particular post is modified:

~~~lisp
(defmethod modified-p ((post <post>))
  (let ((fname (filename post)))
    (and
     (probe-file fname)
     (< (or (ignored-at post) (updated-at post) (created-at post))
        (file-write-date fname)))))
~~~

You see `file-write-date` function there. This logic works really well
if no one touches files, however whenever files get moved or repo gets
clone to anther location, timestamp from it gets distorted and all files
are marked as modified. This is precisely the reason of `ignored-at` field
there. Whenever I want to mark all the files as uptodate I run this
function:

~~~lisp
(defun ignore-all ()
  (let ((ts (get-universal-time)))
    (loop for post in (posts *posts*) do
          (setf (ignored-at post) ts))
    (save-posts)))
~~~

This was one of the first times I used `loop` macro and I really fell in
love with it some time after that.

### Markdown

Last fun bit from the basic implementation is about markdown formatting.
I decided to use it from the vry beginning simply because it's an order
of magnitude better to write then html and still allows to have markup
in place comparing to real plain text.

And of course both vim and emacs have excellent markdown modes and
make it a real pleasure to write.

What could be interesting about writing markdown? Well, to extend it!

I had two extensions planned. First of all, I didn't want to use real
urls for links between the post. If I did, I would be tied to one particular
webservice and migration to another one would be error prone. If all
links in the source code point to other markdown files, that means that
whenever I decided to republish all posts somewhere else, I could have
all the links resolved.

For markdown rendering I used `cl-markdown` library and I'm not sure
if it was the best choice possible. Why? Becase it's source code is
close to inpenetrable to me and documentation is almost non-existant.
Maybe I'm not a good reader, but I took me many evenings to get my
head around library internals and to understand ways to extend it.

For links generation I kept the assumption that I only have one
database open at a time and it live in `*posts*` variable.
`cl-markdown` unfortunately doesn't export any methods it uses for
html generation and I had to [get][markdown] into library package
to extend it the way I want.

Inline elmeents are rendered using `render-span-to-html` generic
function and common lisp rocks again there, because it allows to
add an auxilary method for it and target a specific set of arguments,
that allows to keep method body clean from unnecessary checks:

~~~lisp
(defmethod render-span-to-html :before
    ((code (eql 'inline-link)) body encoding-method)
  (let ((record (cl-journal.db:get-by-fname cl-journal::*posts* (cadr body))))
    (if record
        (setf (cadr body) (cl-journal.db:url record)))))
~~~

What happense is that for links first element in the `body` element
holds a link that will be put in `href`, and in before element we can
do a lookup in the database and replace a link if necessary. I think
it's an amazing level of flexibility with this amount of effort.

Another extension that I wanted to have was about links to the other
blogs. Livejournal has some custom tags for different purposes and
in this case it uses `<lj user="can3p">`, where user attribute holds
the name of a blog. `cl-markdown` allows extensions and in order
to make `{lj-user can3p}` render to a desired tag it's possible to
use an official extension logic:

~~~lisp
(defextension (lj-user :arguments ((name :required)) :insertp t)
  (setf name (ensure-string name))
  (let ((safe-name (html-safe-name name)))
    (ecase phase
      (:parse)
      (:render
       (format nil "<lj user='~a'>" safe-name)))))
~~~

This looks and is indeed easy but it took me quite some time to figure
all the details out.

This part concludes main highlights of the client. Of course there is more,
you can check out amount of [commands][main] supported at the moment.
Some of them like `new` or `last` are there just for convenience to make
it easier to start writing a new post or quickly open the last one in case
of typos, some like `status` or `url` are informative in sense that they
help to query the database and give out information, last group like
`ignore-all` is used to fix the inperfections in file state handling logic.

There is one more group I haven't talked about, which is `fetch`, `merge`
and `remerge` and this is what I'm going to talk about next.





[roswell script]: https://github.com/can3p/cl-journal/blob/master/roswell/cl-journal.ros
[buildapp script]: https://github.com/can3p/cl-journal/blob/master/Makefile
[main package]: https://github.com/can3p/cl-journal/blob/master/src/main.lisp
[cl-brewer]: https://github.com/can3p/cl-brewer
[magic-ed]: https://github.com/sanel/magic-ed
[xml-rpc]: https://www.livejournal.com/doc/server/ljp.csp.xml-rpc.protocol.html
[s-xml-rpc]: https://common-lisp.net/project/s-xml-rpc/
[rpc4cl]: https://github.com/pidu/rpc4cl
[cl-arrows]: https://github.com/nightfly19/cl-arrows
[lj-api]: https://github.com/can3p/cl-journal/blob/master/src/lj-api.lisp
[file-api]: https://github.com/can3p/cl-journal/blob/master/src/file-api.lisp#L35
[markdown]: https://github.com/can3p/cl-journal/blob/master/src/markdown.lisp
[main]: https://github.com/can3p/cl-journal/blob/master/src/main.lisp
