use Test;

plan 4;

is-eqv Supply.collate, (Supply,).Seq, "Supply type object collates to one-element Seq";

my @numeric = Supply.from-list(10...1).collate.list;
is-deeply @numeric, [1, 10, 2, 3, 4, 5, 6, 7, 8, 9], "Supply.collate treats numbers as strings";

my @strings = Supply.from-list("z"..."a").collate.list;
is-deeply @strings, ["a" .. "z"], "Supply.collate sorts strings";

my @mixed = Supply.from-list(flat("a".."d", "A" .. "D")).collate.list;
is-deeply @mixed, [<a A b B c C d D>], "Supply.collate preserves collation order for mixed case";
