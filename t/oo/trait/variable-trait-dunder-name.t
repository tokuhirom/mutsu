use Test;

# A user variable trait whose name starts with `__` is a user trait, not one of
# the parser's internal markers (#14532). Rakudo applies the traits while it
# compiles, so the results are compared at the end. Expected values are the
# `raku` output.

plan 3;

my @seen;
multi trait_mod:<is>(Variable:D $v, :$__x!) { @seen.push: "x" }
multi trait_mod:<is>(Variable:D $v, :$__y!) { @seen.push: "y:$__y" }
multi trait_mod:<is>(Variable:D $v, :$plain!) { @seen.push: "plain" }

my $one is __x;
my $two is __y(5);
my $three is plain;
my $four is __x = 7;
sub f { my $inner is __x; 1 }
f();

is $four, 7, 'a dunder trait with an initializer keeps the value';
is @seen.sort.join(','), 'plain,x,x,x,y:5', 'every handler ran, argument included';
is @seen.elems, 5, 'each exactly once';
