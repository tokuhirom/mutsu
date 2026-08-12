use v6;
use Test;

# `.append` / `.prepend` follow the one-arg rule: a single Range argument
# flattens to its elements (rakudo-verified 2026-08-12). Text::CSV's
# RangeSet.list does `my Int @x; @x.append: $from .. $to` — the typed array
# rejected the unflattened Range with a type-check error.

plan 6;

my Int @x;
@x.append: 1..3;
is-deeply @x, Array[Int].new(1, 2, 3), "typed array append flattens a Range";

my @y;
@y.append: 1..3;
is-deeply @y, [1, 2, 3], "untyped append flattens a single Range";

my @m;
@m.append: (1..3), 5;
is @m.elems, 2, "multiple args are not flattened (one-arg rule)";
isa-ok @m[0], Range, "the Range stays a Range with multiple args";

my @s;
@s.append: "a".."c";
is-deeply @s, ["a", "b", "c"], "string Range flattens too";

my @p;
@p.prepend: 1..3;
is-deeply @p, [1, 2, 3], "prepend flattens a single Range";

done-testing;
