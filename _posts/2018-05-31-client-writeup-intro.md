---
layout: post
title: "Part 1: Intro"
keywords: "common lisp, livejournal, blog, cl-journal"
category: blog
tag: building-blog-client
---

Once I decided to sit down and write a perfect blog client for
me. Since it was not just about the result but also about the process I
chose a language I enjoyed the most and this is how it ended up being
written in Common Lisp. I didn't think I'd get that far and I'd like
to share my experience in the process along with all little details
that I had to go through during the implementation.

**Disclaimer** Please treat this post as an experience report more
than anything else because this is what it is. As a consequence, there
can be a lot of things real lisp wizards will raise an eyebrow on.

**Note 1** I'm glad to hear any feedback. If you have any suggestions
on the text, want to fix a typo or correct my explanation, please
consider opening an issue in the [blog repo][blog repo]. If you think
that client code doesn't do what it has to do, please make a pull
request or open an issue in [client repo][cl-journal repo].

**Note 2** I decided to make all links to the code against fixed sha1
so that future changes do not invalidate them. I'm not sure if I
will frequently revisit this document and I hope that the post
will remain relevant with this approach.

Below you will read a story about writing a blog client for
livejournal.com web service in Common Lisp. As one may note, it sounds
like using a very niche language for a near dead web service. To make
you a bit more surprised I can add that I've been working on it on and
off for a couple of years now.

It used to be very simple but now [cl-journal][cl-journal] is quite
powerful. For example, it handles:

* Any Livejournal compatible service (like Dreamwidth)
* Publishing new posts and updating changed ones
* Post text is written in markdown, many Livejournal fields are supported,
  such as visibility, location or music.
* Fetching posts from remove server and transforming them into markdown
  format.
* Lot's of smaller stuff like links to local files being transformed
  to actual post urls, drafts, pre-commit hooks, password management etc.
* And a lot more!

I've been using Livejournal since 2005 and never had a reason to
switch to anything else. I almost never write on political topics and
don't really care where it's hosted. I don't even need many
connections but do value some and the fact that Livejournal provides
near zero ways to discover other blogs sometimes works more as a
benefit than a drawback, because with the current state of things when
majority of users migrated to Facebook or Instagram, there is not that
much to read there. It's not as engaging as Facebook, and that's
*good*. The other benefit of the service is that it's a very good site
to write long posts like this one and people actually do that and
there is a ton of beautiful content there, if you're lucky to find it
of course.

My main problem with Livejournal always was that I didn't really own
the content in a sense of storage. The blog is mine, ok, but if web
service goes down or account gets blocked I'll lose all my ~1400 posts
written to date without any good way to get them.

From time to time I tried to use other blog platforms or static site
generators (like this site) and my real dream was to combine them and
have sources in markdown locally, so that I can edit them in my
favorite editor and still publish them on a platform that has at least
some social flavor. Blog on Github + Disqus is not social for sure.

I had this idea for some time and somewhere around 2016 got really
burned out by trying to ship yet another web service in my spare time
in addition to those that I ship for my company. To recover I decided
to go in the direction that absolutely excluded any interest except my
own and didn't have any money or fame in sight.  You see now, Common
Lisp and idea about better blogging at the time where blogging is
already out of fashion is almost a perfect match! And this is where it
all started.

## Idea

What does perfect blogging experience look like? It's when you write
the text in your favorite editor, save it, push it to git and it's
published. And it's when you can just grep your posts to remember
something from the past or when you can do bulk updates to all the
posts with system tools that you're used to and get all changes
published at once.

This was my thought process and this is what became a base of
requirements. A client should be able to:

* See new markdown files and publish them
* See modified markdown files and submit changes
* See deleted markdown files and  delete them in the service
* Bonus points if it all happens automagically on git commit/push

That's a simple version of the client that implies that a folder with
markdown files acts as a master database that gets replicated to a
slave which in this case is web service. An obvious drawback here is
that I lose any other ways to update content, be it via service web
interface or it's mobile app. So an even more perfect client should be
able to:

* Fetch any updates from the service
* Transform them into markdown format so that all synced posts can be
  later edited in a usual manner.

One final thought was that since all editing happens in an ordinary
editor, an obvious way to implement client is to make a cli tool for
that. And if you check requirement you'll spot that they resemble
typical git operations a lot and it makes sense to mimic git whenever
it makes sense.

## Common Lisp nontrivialities

When you start working on a really big task, the only thing that can
be said for sure is that there are a lot of unknowns. And if in a
familiar language most of the unknowns lie in the business domain, in
my case a pile of unknowns also waited to be found on the language and
ecosystem side. Some of the things that I thought to be trivial and
that were doable in many languages appeared to be not trivial at all
in Common Lisp and that required a lot of time to grasp to get a full
picture.

Let me go through some of the topics that appeared to be nontrivial.


### Current working directory

This is one such case. It's just one parameter, what can go wrong?
Well, it's two, at least in sbcl. It appears that there is a
distinction between cwd of external commands run by `uiop/run-program`
and internal lisp facilities. Let's make a following experiment:

```bash
# preparation
$ mkdir -p ~/test_cwd/second_folder && cd ~/test_cwd
$ touch test.file
$ touch second_folder/nested.file
$ sbcl
```

and lisp session:

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

So far so good!

    * (uiop/os::chdir "second_folder")

    0
    * (uiop/run-program:run-program  "ls" :input :interactive :output :string)

    "nested.file
    "
    NIL
    0
    * (probe-file "nested.file")

    NIL

Oops. Yes, `probe-file` expects something else.

    * (setf *default-pathname-defaults* #P"second_folder")

    #P"second_folder"
    *  (probe-file "nested.file")

    #P"/home/can3p/test_cwd/second_folder/nested.file"

And to double check.

    * (setf *default-pathname-defaults* #P"/home/can3p/test_cwd/")

    #P"/home/can3p/test_cwd/"
    *  (probe-file "test.file")

    #P"/home/can3p/test_cwd/test.file"
    *  (uiop/run-program:run-program  "ls" :input :interactive :output :string)

    "nested.file
    "
    NIL
    0

And that trailing forward slash in pathname is important, don't lose
it.  As you see, `*default-pathname-defaults*` and `uiop/os::chdir`
are independent and influence different things and to keep it
consistent for both standard library and external calls it's necessary
to call them both.

It's worth saying that this distinction is of particular importance
for repl based development, because when the image starts these two
values are most probably aligned.

Another note is that the behaviour described belongs totally so sbcl,
other implementations may behave differently.

Since I mentioned `uiop/run-program`, let's see how I used it.

### User input

User input is hard in a sense that it's not only about Common Lisp
versus the world it's also about different operating systems, and
it'll be nice if all user input works in emacs repl too.

The simplest ever way exists for yes/no questions and it's as simple
as

```common_lisp
(when (y-or-n-p "Do the thing?") (do-the-thing))
```

But let's say that you want to read an actual input. There is a
`read-line` function that can help. One complication is a prompt
message that you may want to display. Simply formatting it to `STDOUT`
will result in bad results in emacs in a sense that prompt text will
be shown after actual input. To solve this I [found][force output]
that it's necessary to force output on a used stream and now prompt
function looks like this for me:

```common_lisp
(defun prompt-read (x)
  (format *query-io* "~a: " x)
  (force-output *query-io*)
  (read-line *query-io*))
```

And that's not all, because there is another possible user input which
is passwords. Why is it unique? You probably will not want to enter it
clear text. Unfortunately, I didn't find a proper way of doing this
magic inside of lisp and in the end the solution was to use `stty` to
manipulate input visibility and `run-program` and internal bash `read`
function to get user input:

```common_lisp
(defun prompt-read-password (x)
  (format *query-io* "~a: " x)
  (force-output *query-io*)
  (let ((password (run-program "stty -echo; read val; stty echo; echo $val" :output :string :input :interactive)))
    (string-trim '(#\Newline #\Space) password)
    ))
```

Unfortunately this workaround doesn't behave well in Emacs slime
session.

### Storing passwords

Next two are not Common Lisp specific but might be of interest for the
curious ones.  Since I read the password I wanted to store it to avoid
asking for it again and again. However, I wasn't fond of storing it in
clear text and decided to explore system-specific solutions.

Mac os is perfect in this case because of built-in program
[security][mac os security] that allows to store and retrieve
passwords. For Linux (Ubuntu in particular) there are no built-ins,
however, after some search I found [secret tools][libsecret] that
provided similar functionality:

```common_lisp
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
```

Why is `set-password-cwd` signature different? `secret-tool` will read
the password for you and `security` expects it as an input.

### Packaging and running

Since the client is a cli tool, it's really desired to compile the
client system somehow. It's not difficult by itself but I decided to
place an additional restriction there and make it easy to run and
install it both in a dev environment and as an easily installable package.

A task of running the system as a script is perfectly solved by
`roswell` [script][roswell script] and compilation is [done][buildapp
script] with a [buildapp][buildapp]. Since script cannot be used by
buildapp to build a system and roswell and buildapp pass slightly
different parameters to the entry point I ended up moving entry point
logic into a [separate package][main package].

Buildapp itself works amazingly well in case lisp and quicklisp are
set up on the computer, however, I wanted to make it really easy
installable and attaching a binary file to Github release doesn't
sound user friendly for cli tool. The usual way to install a program
on Mac OS nowadays is via Homebrew. A specific requirement for
homebrew formulas is not to use any kind of additional package manager
to build a program, hence quicklisp could not be used as is and I
looked for a workaround.

The result was [cl-brewer][cl-brewer] system that generates a Homebrew
formula with urls pointing to all the dependencies so that formula can
download them all and then use quicklisp and buildapp to make a binary
without any additional downloads.

For Linux distributions I didn't bother to create a package, however
making a deb-package or a snap or whatever looks easy enough. It
just waits for a hero that will do it.

A little note about arguments parsing. There are libraries for this
task in the ecosystem but I ended up using none of them. I wanted
to have a command based cli with some interactivity and the only
things important in this case were first and maybe the second argument.

### Editing

One last bit of user input! What I wanted to do is to allow to invoke
a user editor (set by `$EDITOR` environment variable in case it's
necessary from the cli tool. That also appeared to be not so trivial,
because actual implementation differs by Common Lisp implementation
used.

Fortunately, I found [magic-ed][magic-ed] on Github and it magically
did the right thing (again, not from inside of emacs, which is
unfortunate). Quicklisp didn't have it and I ended up including it in
my code to simplify the build process.

### Pre-commit hook

This one is not really Common Lisp specific, but it turned out to be
pretty useful. What I really wanted to do is to use the client with
git to have all my posts under version control. In this case the best
ever flow looks like this:

```bash
$ cl-journal new post-slug
$ # thanks to magic-ed, an editor opens and you can write your beautiful post
$ git add . && git commit -m "another post"
```

After that new post file along with all the generated metadata should
be included in the commit. A trick here is to include all changes in
the pre-commit hook. Apparently, git allows that. Here is how current
cl-journal pre-commit hook looks like:

```bash
#!/usr/bin/env bash
cl-journal push
git add posts.lisp
```

Of course, I didn't figure everything out beforehand, but it took long
enough to be worth sharing. Now let's get to the client internals.

[blog repo]: https://github.com/can3p/can3p.github.io/issues
[cl-journal repo]: https://github.com/can3p/cl-journal/issues
[cl-journal]: https://can3p.github.io/cl-journal/
[force output]: https://stackoverflow.com/questions/19204332/slime-prints-my-format-calls-only-when-called-function-ends
[libsecret]: https://wiki.gnome.org/Projects/Libsecret
[mac os security]: https://www.netmeister.org/blog/keychain-passwords.html
[roswell script]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/roswell/cl-journal.ros
[buildapp script]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/Makefile
[buildapp]: https://www.xach.com/lisp/buildapp/
[main package]: https://github.com/can3p/cl-journal/blob/5659a99e89cc392fbd56ee3659e70ee8743e2b3e/src/main.lisp
[cl-brewer]: https://github.com/can3p/cl-brewer
[magic-ed]: https://github.com/sanel/magic-ed
