use Test;

plan 8;

# `last` in a deepmap / nodemap block ends the iteration of its own level;
# a nested list ends only itself.
my @a = (1, 2, (3, 4, 5), 6, 7);
is-deeply @a.deepmap({ last if $_ == 4; $_ * 2 }).List, (2, 4, (6,), 12, 14),
    "deepmap: last ends the nested list only";
is-deeply @a.deepmap({ last if $_ == 6; $_ * 2 }).List, (2, 4, (6, 8, 10)),
    "deepmap: last at the top level ends the map";
is-deeply @a.deepmap({ next if $_ == 4; $_ * 2 }).List, (2, 4, (6, 10), 12, 14),
    "deepmap: next skips one element";
is-deeply (1, 2, 3).deepmap({ last if $_ == 2; $_ }).List, (1,), "deepmap over a flat list";
is-deeply [1, [2, 3, 4], 5].deepmap({ last if $_ == 3; $_ * 2 }).Array, [2, [4], 10],
    "deepmap over nested arrays";

is-deeply @a.nodemap({ last if $_ ~~ List; $_.elems }).List, (1, 1),
    "nodemap: last stops at the first list";
is-deeply (1, 2, 3, 4).nodemap({ last if $_ == 3; $_ * 2 }).List, (2, 4),
    "nodemap: last";
is-deeply (1, 2, 3, 4).nodemap({ next if $_ == 3; $_ * 2 }).List, (2, 4, 8),
    "nodemap: next";
