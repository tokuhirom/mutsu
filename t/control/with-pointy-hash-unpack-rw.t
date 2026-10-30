use Test;

# A pointy block that unpacks the topic of `with` / `given` with a hash pattern
# BINDS it: a named `is rw` leaf aliases the element of the topic, so a write
# through it reaches the source. (Found with Red's Mock driver:
# `with %!when-str{$_} -> % (:$times!, :$counter! is rw, :&run!) { $counter++ }`
# never counted a query.)

plan 7;

my %h = a => {counter => 0, times => 1};

with %h<a> -> % (:$counter! is rw, :$times!) { $counter++ }
is %h<a><counter>, 1, '`with` + `% (:$k! is rw)`: ++ writes through';

with %h<a> -> % (:$counter! is rw, |) { $counter += 10 }
is %h<a><counter>, 11, '`with`: += writes through';

with %h<a> -> % (:$counter! is rw, |) { $counter = 7 }
is %h<a><counter>, 7, '`with`: assignment writes through';

given %h<a> -> % (:$counter! is rw, |) { $counter *= 2 }
is %h<a><counter>, 14, '`given` + `% (:$k! is rw)`';

my $seen;
with %h<a> -> % (:$counter! is rw, :$times!) { $seen = "$counter/$times" }
is $seen, '14/1', 'the leaves still read the values';

for %h<a> -> % (:$counter! is rw, |) { $counter++ }
is %h<a><counter>, 15, '`for` + `% (:$k! is rw)`';

my %i = x => {n => 1};
with %i<x> -> $o { my $e := $o<n>; $e += 100 }
is %i<x><n>, 101, 'a binding into the plain `with` topic still aliases';

done-testing;
