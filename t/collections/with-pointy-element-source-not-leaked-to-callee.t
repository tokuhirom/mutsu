use Test;

# From CSS::TagSet (CSS::Properties::Optimizer): `with %h<k> -> \v { f() }` left
# the element pending, so a `with` inside `f` wrote its topic back into `%h<k>`.
plan 3;

role U { }
sub topicalize($a) { with $a { 1 } }

my %h = x => 5;
with %h<x> -> \val { topicalize("it" but U) }
is-deeply %h, {x => 5}, 'pointy with leaves the element alone';

my %g = x => 5;
with %g<x> -> $val { topicalize("it" but U) }
is-deeply %g, {x => 5}, 'scalar pointy too';

my %w = x => 5;
with %w<x> { $_ = 7 }
is-deeply %w, {x => 7}, 'plain with still writes its topic back';
