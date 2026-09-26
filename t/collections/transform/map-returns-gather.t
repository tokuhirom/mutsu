use Test;

# A `.map`/`.grep` callback returning a plain `gather` Seq must leave a
# readable element: static readers (gist, `.flat`, `.raku`, hash
# assignment) cannot force a nested gather themselves (#9584).

plan 11;

is (1,).map({ gather take 5 }).gist, '((5))', 'gist of a mapped gather';
is (1,).map({ gather take 5 }).flat.gist, '(5)', '.flat sees the mapped gather';
is-deeply (1,).map({ gather take 5 })[0], (5,).Seq, 'indexing still works';

my @a = (1,).map({ gather take 5 });
is @a.raku, '[(5,).Seq,]', 'array assignment keeps the gather as one Seq element';

my @b = (1..3).map({ gather { take $_; take $_ * 2 } });
is @b.map(*.elems).join(','), '2,2,2', 'each element is its own Seq';
is @b.map(*.Slip).join(','), '1,2,2,4,3,6', 'slipping the results';

my %h = (1, 2).map({ gather { take "k$_"; take $_ } }).flat;
is-deeply %h, %(k1 => 1, k2 => 2), 'hash assignment from flattened gathers';

# An infinite gather is not forced to the end: it stays lazily resumable.
is (1,).map({ gather loop { take 1 } }).head.head, 1,
    'infinite gather from a map block stays lazy';
is (1,).map({ my $i = 0; gather loop { take $i++ } }).head.head(3).join(','), '0,1,2',
    'resuming a partially pulled infinite gather';

# A `lazy`-marked gather is left alone.
ok (1,).map({ lazy gather take 5 })[0].is-lazy, 'a lazy gather stays lazy';

# The lazy pipeline path reifies gather results too.
is (1..*).map({ gather take $_ }).head(3).gist, '((1) (2) (3))',
    'lazy map over an infinite source returning gathers';
