# How to survive software eating the world

Once I talked with my mother and she told me that a few years ago
many kids were not able to apply to university because their application
should have been handled via a website, which showed them spinner
on the upload and actually never resulted in actual application.

This might sound funny to any developer because we can immediately
guess what could have happened:

* Plain javascript breakage
* Upload functionality was broken for a particular browser
* Job handling the uploads were not deployed even

It's just one example, but everybody has a story where they got into
a sequence of very unfortunate services that were broken in a very
specific way that nicely chains with breakages in other services to
provide an especially miserable experience to a human being.

I argue that that's a direct outcome of the way things are being
built right now. Pareto pricinple, agile methodology, lean movement
are all about doing whatever has the most impact and moving on
to the next big thing combined with a firm belief that software
is there to solve all the things and reinforced by the money incentives
to have as little people in operation as possible.

Do you remember last time you've been chatting with a stupid chat bot
that was almost designed to be useless and wondered why this thing
is there in the first place?  That's why.

What makes the software even more fragile is, surprisingly, the ease
of deployments. If a fix is one deploy away, you're more likely to
deploy a half-baked solution compared to a different extreme of
a space probe launch where you frequently have no chance for updates in
total.
