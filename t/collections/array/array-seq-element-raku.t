use Test;

plan 6;

# A Seq stored as an element of a real Array renders bare in `.raku`: the
# element IS a Scalar container, so the itemization is not information (#17117).
my $s = (1, 2).Seq;

my @f = $s;
is @f.raku, '[(1, 2).Seq,]', 'my @a = $seq';

my @d;
@d.push($s);
is @d.raku, '[(1, 2).Seq,]', '@a.push($seq)';

my @h = $s.item;
is @h.raku, '[(1, 2).Seq,]', 'my @a = $seq.item';

my @e;
@e[0] = $s;
is @e.raku, '[(1, 2).Seq,]', '@a[0] = $seq';

# A List keeps the sigil, and the element itself still shows its container.
is ($s,).raku, '($((1, 2).Seq),)', 'a List keeps the itemization';
is @f[0].raku, '$((1, 2).Seq)', 'the element read out shows its Scalar';
