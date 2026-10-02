use Test;

# #9260: a callback passed through a routine's `&code` parameter, and an
# array passed through its `@values` parameter, keep their writable aliases.
# The callback's `$_` (or `is rw` parameter) is the element container, so a
# write in the callback reaches the `is copy` variable or the caller's array.
# These are the shapes `List::MoreUtils` (`apply`, `indexes`, `pairwise`)
# relies on; the expected values are Rakudo's.

plan 9;

{
    sub apply(&code, @values) {
        @values.map(-> $_ is copy { code($_); $_ })
    }
    is-deeply apply({ $_++ }, 0 .. 3), (1, 2, 3, 4).Seq,
        'a callback mutates an `is copy` topic through a &code parameter';
}

{
    my @values = 0 .. 9;
    sub indexes(&code, @values) { @values.grep(&code, :k) }
    is-deeply indexes({ ++$_ > 7 }, @values), (7, 8, 9).Seq,
        '.grep(&code, :k) through a &code parameter sees the incremented elements';
    is-deeply @values, [1 .. 10],
        '... and the increments reach the caller\'s array through @values';
}

{
    sub pairwise(&code, @a, @b) {
        my @r;
        for ^(@a max @b) -> $i { @r.push: code(@a[$i], @b[$i]) }
        @r
    }
    my @a = 1, 2, 3;
    my @b = 10, 20, 30;
    is-deeply pairwise(-> $a is rw, $b is rw { $a++; $b *= 2; $a + $b }, @a, @b),
        [22, 43, 64], 'a pointy block with two `is rw` params accepts array elements';
    is-deeply @a, [2, 3, 4], '... its first `is rw` writes reach the first array';
    is-deeply @b, [20, 40, 60], '... its second `is rw` writes reach the second array';
}

{
    my @a = 1, 2, 3;
    sub each-rw(&code, @v) { for @v -> $x is rw { code($x) } }
    each-rw(-> $z is rw { $z = 'x' }, @a);
    is-deeply @a, ['x', 'x', 'x'], 'an `is rw` loop variable forwards its alias to a callback';
}

{
    my @h = 1 .. 3;
    sub g(&c, @v) { @v.map(&c).eager }
    g({ $_ *= 10 }, @h);
    is-deeply @h, [10, 20, 30], '.map(&c) through parameters writes back to the caller';
}

{
    my @a = 1, 2, 3;
    sub p2(&code, @a) { (^@a).map({ code(@a[$_]) }).eager }
    p2(-> $x is rw { $x = 0 }, @a);
    is-deeply @a, [0, 0, 0], 'an element read by index inside a nested block stays writable';
}
