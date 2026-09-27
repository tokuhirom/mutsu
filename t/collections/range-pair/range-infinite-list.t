use Test;
plan 8;

# `(1..Inf).List` returns a genuinely lazy List, not the Range itself
# (https://github.com/tokuhirom/mutsu/issues/9783).
my $l = (1..Inf).List;
is $l.^name, 'List', 'infinite range .List reports type List';
is $l.gist, '(...)', 'infinite range .List gists as (...)';
ok $l.is-lazy, 'infinite range .List is still lazy';
is-deeply $l[^5], (1, 2, 3, 4, 5), 'infinite range .List reifies elements on demand';

my $star = (1..*).List;
is $star.^name, 'List', '1..* .List reports type List';
is $star.gist, '(...)', '1..* .List gists as (...)';

# A finite range's `.List` keeps its plain, eager behavior.
my $finite = (5..10).List;
is $finite.^name, 'List', 'finite range .List still reports type List';
is-deeply $finite, (5, 6, 7, 8, 9, 10), 'finite range .List is unchanged';
