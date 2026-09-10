use Test;

plan 12;

# `.cache` on a genuinely-lazy Seq stays lazy (Rakudo's `.cache` reifies and
# caches on demand, it does not force) but the reported TYPE changes to
# `List` immediately, matching `.List`/`.Array`. Only `.^name` (the plain
# `value_type_name` lever) was wrong before this fix -- `.WHAT.^name` already
# read the same context marker.
{
    my $seq = (1…∞);
    is $seq.cache.^name, 'List', '.cache of an infinite Seq reports List';
    is $seq.List.^name,  'List', '.List of an infinite Seq reports List';
    is $seq.Array.^name, 'Array', '.Array of an infinite Seq reports Array';
    ok $seq.cache.is-lazy, '.cache of an infinite Seq stays lazy';
}

# A lazy map/grep pipeline (not a raw sequence spec) goes through a different
# code path (lazy_pipe_preserving_coercion) that must retag the same way.
{
    my $pipe = (1..Inf).map({ $_ });
    is $pipe.^name,       'Seq',  'untagged lazy pipe reports Seq';
    is $pipe.cache.^name, 'List', '.cache of a lazy pipe reports List';
    is $pipe.List.^name,  'List', '.List of a lazy pipe reports List';
    ok $pipe.cache.is-lazy, '.cache of a lazy pipe stays lazy';
}

# An explicitly `.lazy`-marked list keeps its List/cache coercions lazy too.
{
    my $lazy = lazy (1, 2, 3);
    is $lazy.List.^name,  'List', '.List of an explicitly-lazy list reports List';
    is $lazy.cache.^name, 'List', '.cache of an explicitly-lazy list reports List';
}

# Regression: `is-deeply` on two Seq values recurses through
# `multi is-deeply(Seq:D, Seq:D)` -> `.cache` in the vendored Test.rakumod.
# Before this fix `.cache` still reported `Seq`, so the multi re-dispatched to
# itself forever (stack overflow). A finite Seq is enough to exercise the
# dispatch without needing an infinite source.
{
    my $got = (1, 2, 3).Seq;
    my $expected = (1, 2, 3).Seq;
    lives-ok { is-deeply $got, $expected }, 'is-deeply on two Seqs does not recurse forever';
}
