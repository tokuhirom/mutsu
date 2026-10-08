use Test;

plan 5;

# A list literal holding an infinite gather directly must not run it (#11910).
my $l = (1, gather { for 1..* { take $_ } });
is-deeply $l[1][^3].List, (1, 2, 3), 'infinite gather element is lazy in a list literal';

my $p = (:a(gather { for 1..* { take $_ } }), 5);
is-deeply $p[0].value[^2].List, (1, 2), 'infinite gather in a pair element stays lazy';

# A finite gather still renders its values.
is ("a", gather { take 2 }).raku, '("a", (2,).Seq)', 'finite gather element renders via .raku';
is (1, gather { take 2; take 3 }).elems, 2, 'gather element is one item';
is (1, gather { take 2; take 3 }).gist, '(1 (2 3))', 'finite gather element renders via .gist';
