use v6;
use Test;

plan 12;

# Structural expressions carried through the executing finalization
# (ADR-15760 slice 2) keep their meaning.

my @a = 1..5;
is-deeply @a[], [1, 2, 3, 4, 5], 'a zen slice';
@a[0] = 9;
is @a[0], 9, 'an indexed assignment';
my %h;
%h<a> = 1;
ok %h<a>:exists, 'a hash subscript with an adverb';
my @m = [1, 2], [3, 4];
is @m[1;0], 3, 'a multi-dimensional index';
@m[0;1] = 7;
is @m[0;1], 7, 'a multi-dimensional assignment';
"abc" ~~ /(b)(c)/;
is $0 ~ $1, 'bc', 'positional captures';
my $v = 3;
ok 1 < $v < 5, 'a chained comparison';
my $y = ($v = 4);
is $y, 4, 'an assignment used as an expression';
my $n = 'v';
is $::($n), 4, 'a symbolic variable lookup';
is @a>>.abs.join(','), '9,2,3,4,5', 'a hyper method call';
(1, 2, 3) ==> map({ $_ * 2 }) ==> my @out;
is @out.join(','), '2,4,6', 'a feed into a declaration';
my @seen;
for 1..5 { next if $_ == 2; last if $_ == 4; @seen.push($_) }
is @seen.join(','), '1,3', 'next and last';
