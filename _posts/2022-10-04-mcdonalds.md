---
layout: post
category: blog
title: "Mcdonald's and web servers"
---

When you have a hammer, everything looks like a nail they say. That's true to
an extent, but actually some patterns are so generic that both problems and
solutions seem to be the same. If you think otherwise just let me to tell you
a story of one fast food restaurant.

It's quite rare to find an inefficiently running place in the Netherlands and
almost impossible to get into a real mess and the whole restaurant part of
mcdonald's business is based on the efficiency of the operations. Given this I
was quite surprised to get into a restaurant that literally almost collapsed
under the load. It's worth saying that currently 99% of the orders are being
made via the terminals near the entrance.

When I entered the place I has not been initially alarmed by a huge crowd near
the pick up area and I made my order only to realize that not only my order did
not show up on the screen but it looked like there were 13 orders before mine
waiting for their place on the screen.

The kitchen seemed to be in a bad situation:

* There was a pile of hamburgers of all sorts ready for collection
* A guy responsible for drinks kept doing his part and a tray in front of
  him had drinks for something like 30 orders.
* Orders were served in a very slow pace.

Given such a strange mixture I started inspecting the area in hope to find the
source if the problem. Soon it became clear that the root cause was a kids
party! What happened was that parents ordered something like 30 Happy meals and
fulfilling this order cause such a degradation in the throughput that the
restaurant was not able to keep up with new orders anymore.

What made the problem worse was that since kitchen kept preparing orders that
it has not been able to serve in time, all these poor hamburgers caused a lot
of contention and penalized the throughput even more. Not only this, since a
lot of food got cold before it was served and customers waited for way too
long, the restaurant had to deal with chargebacks as well as replacements where
some of the food had to prepared again which meant a lot of waste being
generated instead of a useful output. And since the situation was quite
extraordinary, all the workers were in the kitchen with no staff to clean waste
bins and after they got overflowed it of course took considerably more time to
clean them.

Apparently the workers were doing their job but the workers were doing their
job but the system itself was out of tune. If you think of it, the problem
could have been mitigated early:

* If we're not talking about ad hoc decision, the chain could have trained
  workers to move quicker to achieve vertical scaling
* Some of the workers could have moved to serving the requests instead of
  completing them in order to increase the throughput
* The problem could have been made less severe if the restaurant simply did not
  allow to do such gigantic orders. If they were split into a set of smaller
  ones the load could have been distributed more evenly.
* Alternatively, they could have put rate limiting in place by disabling some
  of the ordering terminals.

What's the morale there? If you do a web service on a bigger scale, make sure
that the rate-limiting is in place, requests are time-scoped and monitoring
is in place to let you know in advance that your service needs some optimization
or scaling. Have fun!
