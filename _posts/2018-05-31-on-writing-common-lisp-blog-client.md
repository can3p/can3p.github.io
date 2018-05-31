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

## Implementation

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
a user editor in case it's necessary from the cli tool. That also apppeared
to be not so trivial, because actual implementation differs by common lisp
implementation used.

Fortunately, I found [magic-ed][magic-ed] on gitub and it magically
did the right thing. Quicklisp didn't have it and I ended up including
it in my code to simplify build process

[roswell script]: https://github.com/can3p/cl-journal/blob/master/roswell/cl-journal.ros
[buildapp script]: https://github.com/can3p/cl-journal/blob/master/Makefile
[main package]: https://github.com/can3p/cl-journal/blob/master/src/main.lisp
[cl-brewer]: https://github.com/can3p/cl-brewer
[magic-ed]: https://github.com/sanel/magic-ed


