---
layout: post
title: "Browser will fix this"
keywords: "html, browser, rendering, forms, errors"
category: blog
---

After working many years in the frontend development area
I really got used to technology which led to some kind of
blindness to the errors that can happen, I don't see certain
kind of errors anymore just because I got the habit of doing
things certain way and it doesnt (usually) get me into trouble.

When you start writing markup you're all about semantics and
compact code. Isn't it beautiful?

~~~ html
<body>
    <header>
        <h1>My blog</h1>
    </header>
    <section>
        <header>
            <h2>Great post!</h2>
        </header>
        <article>My awesome content</article>
    </section>
...

</body>
~~~

Yeah, it is. And maybe it even makes sence if you're working on the site
alone, or you're planning to ship it to the client and run away because this
kind of markup implies selectors like `section header h2 {}`. And while
it looks nice and concise, things start breaking after you develop more than
one page with shared styles and a product owner who always wants to see this
block a little bit different.

The bigger project grows the more effort is required just to keep semantics
on a proper level, which of course affects the velocity. After few more years
you start to understand that the only difference about html tags is that some
of them are inline and others are block-level. No, actually there are just
elements and there is css that can make anything you like. If I were to write
the same markup today, most probably I would come up with something like this:

~~~ html
<body>
    <div class="header">
        <h1 class="header__title">My blog</h1>
    </div>
    <div class="post">
        <div class="post-head">
            <h2 class="post-head__title">Great post!</h2>
        </div>
        <div class="post-content">My awesome content</div>
    </div>
...

</body>
~~~

Unique classnames, selectors with the lowest specificity possible. Doesn't look
beautiful, but I know for sure, that I won't get into troubles with selector clashes,
my css won't consist on 50% of `!important` rules and since `span` and `div`
element don't get any default styles from the browser they usually have an overwhelming
majority on the page.

The second benefit is that I can always move blocks around without breaking too much
stuff, nest blocks into each other etc. Nesting part is a tricky one, because browser
doesn't really think that elements are the same. Here is the code:

~~~ html
<div class="content">
    <p class="content-head">
    We start good
        <div class="reusable">Block</div>
    Ouch
    </p>
</div>
~~~

Apart from the fact that div looks odd inside of the span, what can go wrong?
Here is what browser thinks ([codepen example](http://codepen.io/can3p/pen/RryEZO)):

![ouch](/public/img/2016-01-28-markup-bugs/nested_tags.png)

Yeah, semantics.

The other nesting pitfall that is possible to get into is with forms([codepen](http://codepen.io/can3p/pen/OMZrQP)):

~~~ html
<form id="form1" action="/post">
    <input name="test">
    <form id="i-dont-know-how-i-got there" action="/get">
        <input name="login">
        <input name="password">
    </form>
</form>
~~~

![omg](/public/img/2016-01-28-markup-bugs/nested_forms.png)

The last one is my personal favourite, because the misbehaviour
can be introduced a loong time before you will introduce the changes
that actually trigger the bug:

~~~ html
<div>
<form id="form1" action="/post">
    <input name="test">
    <input name="login">
    </div>
    <input id="password" name="password">
    <span id="spacer"></span>
</form>
</div>
~~~

Mind the closing `</div>` in the middle of the form. It can really happen
if you have a huge project with lots of nested templates. Here is inspector view:

![oh no](/public/img/2016-01-28-markup-bugs/form_div_intersect.png)

You see, last input field falls out of the form. The browser magic is in the fact
that it still belongs to form (check [another codepen](http://codepen.io/can3p/pen/NxMeBr))!

The bug becomes visible if you insert another input dynamically, like this:

~~~ javascript
document.querySelector('#spacer').innerHTML = '<input id="out" name="out">';
~~~

This input won't belong to the form at all ([codepen](http://codepen.io/can3p/pen/OMZray)).

If you think about it, all this absolutely makes sence, because javascript works with
already fixed DOM tree and the element is inserted outside of the form, but it can take
quite some time to spot this in a huge codebase.

Forgot to mension, ids are here only for test purposes and ideally should be used only
in situations where html spec wants them (like `<label for="" />`).
