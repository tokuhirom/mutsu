use v6;
use Test;

# A slice assignment must distribute a NON-i64 range (string / Rat / .succ-driven)
# across the slice, the same way an i64 range or a comma list already does.
# Previously `@a[^10] = 'a'..'z'` stored the whole Range in slot 0.

plan 8;

{
    my @letters;
    @letters[^10] = 'a'..'z';
    is-deeply @letters, ['a','b','c','d','e','f','g','h','i','j'],
        'positional slice distributes a string range';
}

{
    my @l;
    @l[0, 1, 2] = 'a'..'z';
    is-deeply @l, ['a','b','c'], 'explicit-index slice takes a range prefix';
}

{
    my @l;
    @l[^4] = 'aa'..'zz';
    is-deeply @l, ['aa','ab','ac','ad'], 'two-char string range distributes';
}

{
    my %h;
    %h<a b c> = 'x'..'z';
    is %h<a>, 'x', 'hash slice string range - first';
    is %h<b>, 'y', 'hash slice string range - second';
    is %h<c>, 'z', 'hash slice string range - third';
}

# i64 range and comma list still work (no regression).
{
    my @l;
    @l[^3] = 1..100;
    is-deeply @l, [1, 2, 3], 'i64 range slice still distributes';
}

{
    my @l;
    @l[^3] = 10, 20, 30, 40;
    is-deeply @l, [10, 20, 30], 'comma list slice still distributes';
}
