use Test;

# Left-exclusive sequence operators `^...` and `^...^`: same series as
# `...`/`...^` but the first element is dropped (`5 ^... 0` == `(4 3 2 1 0)`).
# mutsu supported `...` and `...^` but greedily parsed the `^...` prefix as the
# `^..` range and failed. Found via the real-distribution compat sweep
# (Astro::Utils, docs/dist-compat-sweep.md), which does `(c.elems ^... 0)`.

plan 10;

is-deeply (5 ^... 0).List, (4, 3, 2, 1, 0), '^... drops the first element (end inclusive)';
is-deeply (5 ^...^ 0).List, (4, 3, 2, 1).List, '^...^ drops first and excludes end';
is-deeply (1 ^... 5).List, (2, 3, 4, 5), '^... ascending';
is-deeply (1, 3 ^... 11).List, (3, 5, 7, 9, 11), '^... with a two-element (comma) seed';
is-deeply (1, 3 ^...^ 11).List, (3, 5, 7, 9).List, '^...^ with a comma seed';

# unchanged siblings
is-deeply (5 ... 0).List, (5, 4, 3, 2, 1, 0), '... unchanged';
is-deeply (5 ...^ 0).List, (5, 4, 3, 2, 1).List, '...^ unchanged';
is-deeply (0 ^.. 5).List, (1, 2, 3, 4, 5), '^.. range unchanged (not a sequence)';

# lazy infinite left-exclusive sequence stays lazy
is-deeply (1 ^... Inf)[^4], (2, 3, 4, 5).Seq[^4], '^... to Inf stays lazy';

# a method-style seed (the Astro::Utils shape)
{
    my @c = <a b c d>;
    is-deeply (@c.elems ^... 0).List, (3, 2, 1, 0), '^... over a method-call seed';
}
