use Test;

# `$0:exists` is `$/[0]:exists`, like `$<foo>:exists` is `$/<foo>:exists`.
plan 8;

"abc" ~~ /(a)(x)?/;
is $0:exists, True, '$0:exists for a matched group';
is $1:exists, False, '$1:exists for a group that did not take part';
is ($0:exists eq 'True'), True, 'inside parentheses, before an infix';
is $0:!exists, False, '$0:!exists';
is $5:exists, False, 'a group beyond the last one';
is ($1:!exists), True, 'negated, in parentheses';
is ("b" ~~ /(b)/ ?? ($0:exists) !! False), True, 'in a ternary branch';

my %h = a => 1;
is %h<a>:exists eq 'True', True, 'the hash form is unchanged';
