---
layout: post
title: "Complexity tower and a holy grail of web development"
keywords: "web development"
category: blog_draft
---

I've been thinking about it for a while. Sometimes I was so bad that I wanted
to quit programming, sometimes it got better and I though it makes sense again
(sort of). But in reality, it's bad, it's really really bad.  After watching
Jonathan's Blow video ["Preventing the collapse of Civilization"][collapse] I
decided to finally convert some words into bytes. What am I talking about?

Let's just observe how a perception of complexity kept changing with
time in regards to different concepts of web development.

If you remember good old days, it was a pretty easy thing to make a website.
You edit an `index.html` page till it looks fine, then you copy it to a
cheap hosting with any tool you like and you're done. If you really wanted
to have some interactivity you could go wild and write some perl or PHP.

Maybe you also remember how local development worked back in the days. My
weapon of choice back then was [WAMP][wamp] or something similar.

And that was it, mostly. There were several not so easy things with this
approach. For example, you had to manually fiddle with the database,
and CSS + HTML was a special kind of art due to amazing number of bugs
and inconsistencies across different platforms.

Then the pendulum started it's swing.

A couple of great things that happened back then was appearance of jQuery
and other similar libraries like mootools and standards movement together
with bulletproof css frameworks like Twitter Bootstrap that allowed
an average developer to abstract away from the browser internals and
just code a website.

On the other side of the stack framworks like Zend, Django or Rails turned
backend development on it's head and raised productivity many times, because
data management, templates and routig became dead simple.

It was cool and nice, however with the rise of complexity on the frontend
jQuery was not enough anymore. It was dead simple to do stuf like
`$('#block').slideUp()` or even `$('#popup').show()`, but if you wanted
to wire up something really complex, all that rather quickly turned into
a spaghetti. Just remember all that `script.js` files that contained
everything as one wall of text. The same goes for template engines -
it was quite simple to split them into a couple of files, but a big
number of templates was troublesome - most of them were text based (just
remember that time when you spent hours hunting a missing closing tag),
and scope was a real problem: with many layers it was harder to say
where you got this specific variable from and when it'll disappear.

Web development was booming and everyday someone popped up with a new idea on
how to do it in a simple way and the whole web-development ecosystem turned
into a "Hey, check out this cool hack" kind of community.

What happened next was containers, rise of single page applications,
dynamic data formats like json or yaml (only old guys love xml, yes)
and proper package management.

They made it so simple to share and compose, that you could barely
resist. Now you could get even more efficient with what you do, especially
with libraries and frameworks like React or Angular or, later Vue.

The only catch was that the day when you could just save a file and upload
it to a cheap hosting was gone. Build step became a necessity and as
things progressed even more, React and friends became a sort of default
solution for any kind of application. You could easily do time machine
kind of tricks, because your state was not managed with Redux, but a
simple animation was not so simple anymore and having a popup was not
something you would really like to do.

Moreover since your frontend turned into a full js app, serving an html
page became something that you really need to think about. In the
end a popular solution was to run a node.js app alongside with a
backend app and consume backend apis both from the node.js app and from
the frontend, we got isomorphic apps.

It was really cool, the only thing that you now had two runtimes instead
of one, had to monitor and understand them and their interaction.
The pain was not immediate, especially if you run them locally, but
docker and docker compose made it so dead simple to run a lot of stuff
in an easy manner, that you could hardly understand how such setups
worked and you had to in the end because out of many compomnents the
probability was quite high to have at least on of them brokend at
least in one of the many environments.

And then we got kubernetes that made it quite simple to run such
complex apps, but it was a beast by itself and now you had a magical
thing running and it was wired up by a pile of yaml files that
looked so easy to write but were fantastically hard to reason about
when you had a lot of them.

Now you could do a lot of stuff. The only thing that you had trouble
with was to get a development environment and speed like it was
with WAMP stack.

One more side effect that we got was that backend frameworks somehow
lost a lot of ground. Since everything is an spa, you can't simply
use Django forms now, you'll have to leave that alone and do your
form with react, which is not as simple. And don't forget to do
validations! And all this had to be joined with an api that you now
had to write instead of just writing  post handler there.

The other moment is a cloud push. It works amazingly well and
allows you to run the craziest setup if you manage to wire it up
but what it makes challenging is having an idea about your
bill next month.

There was so much frustration at this point that it's quite reasonable
that the whole frontend world embraced Typescript to get a bit
more sanity in everything happening.

And btw, I'm not even talking about all the regulations that came
our way - think about all the EU regulations that made it kind of
safer for us, except that everybody agrees on anything anyway just
to get thought that pile of cookie related popups to read an
article. One can say that we're in a better place as customers now,
but I think that all big guys will implement/find a workaound
to it anyway, so in reality we just drastically increased the load
on all small companies that have to deal with it. When did you
ask a random website to delete all your data last time?

Now, I think it's time to make a step back and think for a bit.

Everything I mentioned so far was really necessary for some projects.  I mean
it - docker is really powerful, you can do wonders with new js framworks,
however what's not there is the simplicity of doing things in a way that was
achievable.

And while it's really nice that all new stuff takes literally seconds
to start, it's not what we're doing every day. What we're doing is
maintenance and iterations on a codebase. If your setup requires
200mb of javascript and some more python to run with some yaml on
top, it's not simple to maintain and quite frustrating to deploy. No
wonder developers go for solutions like Heroku just to relieve some
pain.

And while all big companies require these tools we should not. If
what you're building is a not so big website, you should be able
to do it in a simple to maintain and deploy manner with as little
moving parts as possible, while still being able to take an advantage
of last years of web development.

It's time to think again and come up with and promote a solution that
will be dead simple in implementation and still allow to evolve to a
complex one if necessary but not before.


[collapse]: https://www.youtube.com/watch?v=pW-SOdj4Kkk
[wamp]: http://www.wampserver.com/en/
