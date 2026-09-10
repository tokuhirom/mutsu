use Test;

plan 6;

ok (set <a b c>) ~~ (mix <a b c>), "Set smartmatches equivalent Mix";

is (a => 0).Mix.keys.elems, 0, "zero-weight Pair disappears in .Mix coercion";

my %counts = mix <a b o p a p o o>;
is-deeply %counts, { :2a, :1b, :2p, :3o }, "assigning mix to %var flattens keys and counts";

my %bound := mix <a b c b>;
isa-ok %bound, Mix, "%var bound to mix keeps Mix container";
is %bound<b>, 2, "bound Mix hash lookup works";
throws-like { %bound = mix <a b> }, X::Assignment::RO, "bound Mix hash variable is readonly";
