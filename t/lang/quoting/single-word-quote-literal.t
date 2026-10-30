use v6;
use Test;

plan 6;

# A one-word quote is the word itself (an allomorph for `<...>`), not a List,
# and, like any word list, it is a literal a user's `sub list` cannot intercept.
sub list($a, $b) { 'user list' }

is <a>.WHAT.gist, '(Str)', '<a> is a Str';
is <1>.WHAT.gist, '(IntStr)', '<1> is an IntStr';
is <1/2>.WHAT.gist, '(Rat)', '<1/2> is a Rat';
is qw<a>.raku, '("a",)', 'qw<a> is a one-element List';
is (qw<a b c>).elems, 3, 'a word list is not routed through a user sub list';
is (<a>,).raku, '("a",)', 'a one-word quote inside a list';
