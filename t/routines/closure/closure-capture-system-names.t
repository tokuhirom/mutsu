use Test;
use MONKEY-SEE-NO-EVAL;

# A closure capture keeps every visible system name (types, constants,
# dynamics, uppercase lexicals). A wide scope's system names are shared by
# every capture of it through a memoized layer instead of being copied into
# each one (#9170). These pin that the shared layer is the right binding at
# the right time: that a rebinding between two captures is seen, that a
# closure created inside a closure still sees both scopes, and that `gather`
# bodies see the same names.

plan 16;

# Forty top-level classes make the mainline tier wide enough to be layered.
EVAL (^40).map({ "class Wide$_ \{ method n \{ $_ \} \}" }).join("; ") ~ '; 1';
my class Point { has $.x; method double { Point.new(x => $!x * 2) } }
constant LIMIT = 3;
my $Upper = 'u';
my $*DYN = 'outer-dyn';

my @c;
for ^3 -> $i { @c.push: -> { Point.new(x => $i).double.x } }
is @c».().join(','), '0,2,4', 'a lexical class is visible to closures made in a loop';

is (-> { LIMIT * 2 })(), 6, 'a constant is visible to a closure';
is (-> { $Upper ~ $Upper })(), 'uu', 'an uppercase lexical is visible to a closure';
is (-> { $*DYN })(), 'outer-dyn', 'a dynamic variable is visible to a closure';
is (-> { ::('Wide7').n })(), 7, 'a type looked up by name inside a closure';

# A rebinding between two closure creations is seen by the later one.
my $Bound := 1;
my $before = -> { $Bound };
$Bound := 2;
my $after = -> { $Bound };
is $after(), 2, 'a closure created after a rebinding sees the new binding';

# A mutated uppercase lexical is one container shared by every closure.
my $Counter = 0;
my @readers = (^3).map({ $Counter = $_; -> { $Counter } });
is @readers».().join(','), '2,2,2', 'closures over a mutated uppercase lexical see its final value';

# A fresh uppercase declaration per iteration is one binding per closure.
my @fresh = (^3).map({ my $Fresh = $_; -> { $Fresh } });
is @fresh».().join(','), '0,1,2', 'a per-iteration uppercase declaration is captured per closure';

# A closure created inside a closure sees both the inner and outer scopes.
my &outer = -> $k {
    my $Inner = $k * 10;
    -> { $Inner + LIMIT + Point.new(x => 1).x }
};
is outer(2)(), 24, 'a nested closure sees its creator and the mainline';

# The topic is the closure's own, not the creating scope's.
my @topics = (do for <a b> { (1, 2).map({ $_ }) }).flat;
is @topics.join(','), '1,2,1,2', 'a block topic is not inherited from the loop';
is (* + 1)(41), 42, 'a WhateverCode binds its own topic';

# A dynamic rebound by the caller wins over the captured one.
sub with-dyn(&code) { my $*DYN = 'caller-dyn'; code() }
is with-dyn(-> { $*DYN }), 'caller-dyn', 'a dynamic resolves against the caller';

# gather bodies see the same names, and their writes still reach the outer scope.
my @g = gather { take Point.new(x => LIMIT).x; take $Upper; take $*DYN };
is @g.join(','), '3,u,outer-dyn', 'a gather body sees types, constants, uppercase lexicals and dynamics';
my $Total = 0;
my @h = gather { for ^3 { $Total += $_; take $_ } };
is @h.elems, 3, 'the gather ran';
is $Total, 3, 'a gather body write to an uppercase lexical reaches the outer scope';
my @nested = gather { take (-> { ::('Wide3').n + LIMIT })() };
is @nested[0], 6, 'a closure created inside a gather body sees the mainline';
