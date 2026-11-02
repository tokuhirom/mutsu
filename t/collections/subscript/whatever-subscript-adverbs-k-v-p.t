use Test;

plan 11;

# `*<a>:k` / `:v` / `:p` (value adverbs on a WhateverCode subscript) prime a
# WhateverCode around the adverb call instead of evaluating eagerly (#15446).
my %h = a => 1;
my @a = 5, 6;

is-deeply (%h,).map(*<a>:k).List, ('a',), '*<a>:k';
is-deeply (%h,).map(*<a>:v).List, (1,), '*<a>:v';
is-deeply (%h,).map(*<a>:p).List, (:a(1),), '*<a>:p';
is-deeply (%h,).map(*<a>:exists).List, (True,), '*<a>:exists still works';
is-deeply (%h,).map(*<b>:!exists).List, (True,), '*<b>:!exists still works';
is-deeply (@a,).map(*[1]:k).List, (1,), '*[1]:k';
is-deeply (@a,).map(*[1]:v).List, (6,), '*[1]:v';
is-deeply (@a,).map(*[1]:p).List, (1 => 6,), '*[1]:p';

my &f = *<a>:v;
is f(%h), 1, 'the WhateverCode is a callable';
is-deeply %h<a>:k, 'a', 'the plain adverb form is unchanged';
is-deeply @a[1]:p, 1 => 6, 'also on an array';
