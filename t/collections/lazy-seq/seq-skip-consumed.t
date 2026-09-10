use Test;

plan 8;

# .skip on an uncached Seq consumes the original: re-iterating it throws.
{
    my $s := (1..3).Seq;
    my $skipped := $s.skip;
    throws-like { @$s }, X::Seq::Consumed, 'Seq.skip consumes the original Seq';
    is-deeply @$skipped, (2, 3), 'skipped Seq has the right content';
}

# .skip(n) likewise consumes the original.
{
    my $s := (1..3).Seq;
    my $skipped := $s.skip: 2;
    throws-like { @$s }, X::Seq::Consumed, 'Seq.skip(n) consumes the original Seq';
    is-deeply @$skipped, (3,), 'skipped(n) Seq has the right content';
}

# Array-contextualizing a Seq (@$s) caches it, so it can be read repeatedly.
{
    my $s := (1..3).Seq;
    is-deeply @$s, (1, 2, 3), 'first @$s read works';
    is-deeply @$s, (1, 2, 3), 'second @$s read works (cached)';
}

# A cached Seq is not consumed by .skip.
{
    my $s := (1..3).Seq;
    @$s; # cache it
    is-deeply $s.skip.List, (2, 3), '.skip on a cached Seq still works';
    is-deeply @$s, (1, 2, 3), 'cached Seq survives .skip';
}
