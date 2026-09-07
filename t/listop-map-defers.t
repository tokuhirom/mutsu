use Test;

# ADR-0058 step 3: the listop `map &f, @xs` form defers exactly as the method
# form does. Until it did, the listop answered a `List` where rakudo answers a
# `Seq`, ran its side effects at the call instead of at first consumption, and
# let a `die` inside the callback be caught by a `try` that merely enclosed the
# `map` call.

plan 10;

is (map { $_ * 2 }, 1..3).^name, 'Seq', 'the listop map answers a Seq';

{
    my $log = '';
    sub produce { my $s = map -> $x { $log ~= "RAN"; $x }, 1..3; $log ~= "T"; $s }
    my $seq = produce();
    is $log, 'T', 'the callback has not run when the listop map returns';
    is $seq.List.elems, 3, '... the pull produces every element';
    is $log, 'TRANRANRAN', '... and the callback ran at the pull, not at the call';
}

{
    # A `die` inside the callback escapes a `try` that only lexically encloses
    # the `map`, because the callback runs later, at the consuming statement.
    sub produce-dying { my $s = try { map { die "boom" }, 1..2 }; "reached-tail|" ~ $s.defined }
    is produce-dying(), 'reached-tail|True', 'the enclosing try does not catch a not-yet-run callback';
}

is (map { $_ + 1 }, 1..3).List.raku, '(2, 3, 4)', 'the elements are the mapped ones';
is (map { $_ }, ()).List.raku, '()', 'an empty source produces an empty Seq';

{
    my @out;
    for map { $_ * 5 }, 1..3 { @out.push: $_ }
    is @out.raku, '[5, 10, 15]', 'a `for` over the deferred listop map iterates it';
}

# Pure-value readers cannot pull a deferred Seq, so a consumer that reads the
# callback's result through `as_items` needs the ADR-0058 reify guard.
# `categorize`'s mapper is one: without it the whole categorization came out
# empty (`roast/S32-list/categorize.t`'s multi-level row).
is categorize({ map { $_ + 10 }, .comb }, 12, 34).raku,
    '(my Mu %{Mu} = 11 => $[12], 12 => $[12], 13 => $[34], 14 => $[34])',
    'a `categorize` mapper whose body is a listop map is forced';

is classify({ (map { $_ }, .comb).head }, 12, 34).raku,
    '(my Mu %{Mu} = "1" => $[12], "3" => $[34])',
    '... and so is a `classify` mapper';
