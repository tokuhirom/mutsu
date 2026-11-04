use v6;
use Test;

plan 5;

# A capture literal `\(...)` is carried through finalization as the parser built
# it (ADR-15760 slice 2).

sub f(|c) { c.raku }
is f(1, :a), '\(1, :a)', 'a slurpy capture parameter';
is \(1, 2).raku, '\(1, 2)', 'a positional capture literal';
is \(:x(1)).raku, '\(:x(1))', 'a named capture literal';
my $c = \(1, "a");
is $c.raku, '\(1, "a")', 'a capture literal bound to a scalar';
is \((3, 4)).elems, 1, 'a capture literal holding a parenthesised list';
