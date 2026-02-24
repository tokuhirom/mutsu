use Test;

plan 4;

is-eqv Supply.list, (Supply,), "Supply type object coerces to one-element list";

my @a;
for Supply.from-list(2..6).list { @a.push($_) }
is-deeply @a, [2..6], "Supply.list values can be iterated";

my @b = Supply.from-list(42..50).list;
is-deeply @b, [42..50], "Supply.list values can initialize arrays";

isa-ok Supply.from-list(2..6).list, List, "Supply.list returns List";
