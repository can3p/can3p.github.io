---
layout: post
title: "Perl is nice"
keywords: "perl, syntax"
category: blog
---

There are many trendy things in wht wild and perl is definitely not one of them.
Many blame it to be write-only, and it really has some [ugly parts](http://stackoverflow.com/questions/35227290/determining-if-a-perl-scalar-is-a-string-or-integer),
there are things where it definitely shines. There are lots of parts that make
data juggling easy.

## Lists

My personal favourite is a list concept of the language. By default all lists are squashed
to one level and every function receives nothing but a list of arguments. All hashes are lists
with odd values used as keys and even values used as values. And in the end,
`=>` operator used in hash definition is absolutely equal to comma operator.

If you want to get a nested structure you should use references.

This is absolutely awesome concept that makes so many operations compact and natural that
other languages like javascript should be really jelaous about it, because it allows to get
things like destructuring trivially and makes conversions between lists and hash-map natural.

First of all, there is no nesting:

~~~ perl
(1, 2, 3)


# equivalent
(1, (2, 3))
~~~

It's not only about raw lists, it's about lists as a type, so all this is the same:

~~~ perl
my @arr = (2, 3);

(1, @arr)

sub func { return (2, 3); }

(1, func() )
~~~

Yeah, any expressions can do

~~~ perl
(1, ( $all_good ? (2, 3) : () ))
~~~

Since hash map is initialized with the same lists (it's stored as a hash map structure actually), all tricks still apply, the only restriction is
that number of list items is even.

~~~ perl
my %hash = ( some_key => some_value, get_options() );
~~~

All this allows to have idiomatic way to optionaly set key to hash:

~~~ perl
my %options = (
        required_option => 1,
        ( $condition_holds ?  (optional_key => 1) : ())
        );
~~~

Or use map to transform list to a hash.

~~~ perl
my %hash = map { $_ => 1 } @keys;
~~~

What happens is that we take a list and use map on it, where block is executed with every
element and returns a list `($item_of_list, 1)`. In the end all returned lists are squashed
to a single one which is used to initialize a hash. As a result we have a hash of all members
of a list as keys and ones as values.

It's particularly cool comparing to javascript where there is no way to achieve the same level
of elegance:

~~~ javascript
// We can do like this:

var hash = {};
list.forEach(function(item) { hash[item] = 1; });

// or like this:

var has = list.reduce(function(acc, value) {
    acc[value] = 1;
    return acc;
    }, {});
~~~

For optional keys it's the same level of awkwardness:

~~~ javascript
var options = {};

if (condition_holds) { options[key] = 1 };
~~~

While not a hard thing to do, it pollutes code with unnecessary variable declarations or loops
that do very simple things and distract attention for the main logic.

Now, all this applies to any function calls, because what we pass is just a list of arguments:

~~~ perl
func(1, 2, 3)

# the same

my @a = (2, 3);
func (1, @a);
~~~

Inside function we can parse arguments in any way we want:

~~~ perl
sub func {
    my ($arg, @list) = @_; # one argument and the rest as a list

    # we can call this like func(1, key => value) for example
    my ($arg, %hash) = @_; # one argument and the rest as a hash
}
~~~

As you see, we get simple form of destructuring easily with a natural
uniform syntax. Perl obsession with list went even further. E.g. you can
set multiple keys of the hash at the same time:

~~~ perl
@hash{qw(key1 key2)} = (1, 2);
~~~

As you see, this pattern allows to cut out a lot of boilerplate code and
provides really effective tools for data structure manipulation.

## Default variables

This is one of the features the brought perl a fame of write-only language. The reason is that
It's used for most language constructs and can make the code really dense and almost unpenetrable
to anyone not so similar with perl.

I'm not aware of any other language that has default variables but with a wise use code actually looks
cleaner. There are two main variables that are used as default ones - `$_`  and `@_`, which are
scalar value (number, string, reference) and list.

All function arguments in `@_`:

~~~ perl
sub func {
    my ($arg1, $arg2) = @_;
}
~~~

All loops and operators like `map` or `grep` make `$_` point to current item of a list

~~~ perl
for (@arr) {
    print $_;
}

my @doubles = map { $_ * 2 } @arr;
~~~

If you write this type of code in any language that does not have this feature you'll most probably end up with lots
of variables named as `item` or something alike.

What's more some functions and operators work with `$_` if no explicit argument was provided, and here is the place
when unfamiliar people can get really confused.


~~~ perl
for (@lines) {
    chomp;
    s/abc/def/g;
}

# the same
for my $line (@lines) {
    chomp $line;
    $line =~ s/abc/def/g;
}
~~~

Looking at this example you may see that perl really doesn't mind mutating existing variables. This again can save some keystrokes
but makes it even easier to shoot yourself in a foot.


These are two features unique to perl as a language that allow to do lot's of tricks that make code dense and expressive at the cost
of additional context that any developer should be aware of to successfully read the code. I don't think that it's a bad thing by
itself but most language developers chose different path to provide everything explicitly so decrease learning curve.

Big thanks to Oleg Komarov for reviewing this post.
