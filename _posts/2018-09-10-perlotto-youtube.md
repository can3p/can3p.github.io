---
layout: post
title: "Perlotto - youtube support"
keywords: "javascript, node.js, electron"
category: blog
---

One more update about [Perlotto][1] app (last update is [here][2]). I decided
to work on it again, because of annoyance I felt from using youtube in web
browser. It's just dumb to go from a decent players we all had to a web
page that doesn't work with multimedia keys to say at least.

Integration appeared to be quite easy and now it's more or less trivial to
extend perlotto to work with your favorite service (pull requests are welcome, btw).

The whole integration part is split into several things that need to be done
to make service work:

* How to understand that player page is loaded
* How to understand that player is playing
* How to do play/pause ot switch to the next or previous track
* How to get information about current song.

The last one appeared to be the trickiest, and the reason is that the only
way to get this data is to parse title and description, which are free form.
For now I decided to go with simple spitting of the title string, but I'm
totally sure, that it can be done hundred time better.

One part that I haven't mention in the list is ads. I'm more or less fine
with them, but it appears the without mblock origin youtube plays ad video
after every second song and it was annoying to extend that I decided to cut
it.

Electron provides a quite convenient api for this usecase, it's possible
to control and intercept requests on every stage of the lifecycle. The only
thing that's tricky is how to setup filters, because filter argument looks
very misleading. After some trial and error I ended up with this code:

~~~javascript
    const filter = {};
    function initFilters() {
        session.defaultSession.webRequest.onBeforeSendHeaders(filter, (details, callback) => {
            callback({cancel: checkFilters(details.url)});
        })
    }
~~~

Calling this function will force electron to pass every single request via the
callback, where the actual filter lives:

~~~javascript
    function checkFilters(url) {
        return ad_filters.reduce(function(acc, filter) {
            return acc || !!filter.exec(url);
        }, false);
    }
~~~

`ad_filters` is an array with regexes that are applied to the url, any match
leads to the blocking of the request (full code [there][3]).

Current support is rudimentary and can certainly be improved. Some of the
possible features are:

* Smart track info discovery. With some more logic it's possible to achieve
  much better results with decoding track, artist and album title.

* Full albums handling. Putting full albums on youtube is really popular now,
  and most of the times you can see a full tracklist with timestamps in description
  or one of the comments. It would be a reall dealbreaker to enable perlotto
  to read this informaton and to integrate it with all the media keys and track
  information.

## Outro - spotify

Another service that I decided to integrate was spotify. All the steps seemed
to be pretty easy, except that I've found out that spotify web player doesn't
work in the perlotto due to abscence of widevine plugin.

Adding support required much more work  - it should be downloaded separately
and version of the plugin should match version of the chrome browser, which
sounds like a lot of troubles without real benefit since I'm not a frequent
spotify user.

Maybe I'll do it some day, but if you're interested, please don't stop yourself
from doing a pull request!

[1]: https://github.com/can3p/perlotto
[2]: /blog/2017/06/25/electron-2/
[3]: https://github.com/can3p/perlotto/blob/master/main.js
