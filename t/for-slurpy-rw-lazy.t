use Test;

plan 9;

# `is rw`/`is raw` on a slurpy parameter aliases each caller argument, so body
# mutations flow back to the caller's variables (S04-statements/for.t 90-92).
{
    sub incr1 (*@v is raw) { @v[0]++; @v[1]++; };
    sub incr2 (*@v is raw) { for @v { $_++ } };
    sub incr3 (*@v is raw) { for @v -> $x is rw { $x++ } };
    my ($a, $b) = (0, 0);
    incr1($a, $b);
    is-deeply [$a, $b], [1, 1], 'direct element mutation of raw slurpy writes back';
    incr2($a, $b);
    is-deeply [$a, $b], [2, 2], 'for @slurpy { $_++ } writes back';
    incr3($a, $b);
    is-deeply [$a, $b], [3, 3], 'for @slurpy -> $x is rw { $x++ } writes back';
}

# Three distinct sources, written independently.
{
    sub bump (*@v is raw) { @v[0] += 10; @v[2] += 30; };
    my ($x, $y, $z) = (1, 2, 3);
    bump($x, $y, $z);
    is-deeply [$x, $y, $z], [11, 2, 33], 'sparse writes to distinct sources';
}

# An array variable passed to a raw slurpy aliases its elements.
{
    sub incrA (*@v is raw) { @v[0]++; @v[1]++; };
    my @arr = (10, 20);
    incrA(@arr);
    is-deeply @arr, [11, 21], 'array-source raw slurpy writes back to elements';
}

# A non-lvalue argument to a raw slurpy is simply read-only (no write-back, no error).
{
    sub readonly (*@v is raw) { @v[0] + @v[1] };
    is readonly(3, 4), 7, 'raw slurpy with literal args reads correctly';
}

# `lazy for` does not run its body until the resulting Seq is consumed, and
# does no more work than required (S04-statements/for.t 105/107).
{
    my $loops = 0;
    my @values = lazy for ^10 { $loops++; $_ }
    is $loops, 0, 'lazy for does not execute until asked for values';
    is-deeply @values[^5].List, (0, 1, 2, 3, 4).List,
        'lazy for produces correct values on demand';
    is $loops, 5, 'lazy for does no more work than required';
}
