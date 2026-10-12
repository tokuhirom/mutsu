use Test;

# #11242: several threads reading one `.cache`d Seq that nobody has reified
# yet. The first reader pulls it; the others must wait for that pull rather
# than report "The iterator of this Seq is already in use/consumed".
# (Rakudo 2026.07 itself loses elements on the second block and can crash
# on the third; mutsu serializes the pulls, so every reader sees them all.)

plan 4;

{
    my @bad;
    for ^10 {
        my $s = (1..2000).map(* + 1);
        $s.cache;
        my @r = await (^8).map: { start { $s.elems + $s[1999] } };
        @bad.push: @r.join(',') unless all(@r) == 4001;
    }
    is @bad.elems, 0, 'concurrent .elems / subscript on a cached map Seq agree';
}

{
    my @bad;
    for ^10 {
        my $s = (1..500).map(* * 2);
        $s.cache;
        # Subscripts pull growing prefixes from several threads at once.
        my @r = await (^8).map: -> $t { start { [+] (^50).map({ $s[$t * 50 + $_] }) } };
        my @want = (^8).map: -> $t { [+] (^50).map({ ($t * 50 + $_ + 1) * 2 }) };
        @bad.push: @r.join(',') unless @r eqv @want;
    }
    is @bad.elems, 0, 'concurrent prefix pulls of a cached map Seq keep every element';
}

{
    my class Count does Iterator {
        has $.n = 0;
        method pull-one() { $!n < 300 ?? ++$!n !! IterationEnd }
    }
    my @bad;
    for ^10 {
        my $s = Seq.new(Count.new);
        $s.cache;
        my @r = await (^8).map: { start { $s.sum } };
        @bad.push: @r.join(',') unless all(@r) == 45150;
    }
    is @bad.elems, 0, 'concurrent reads of a cached Seq.new(iterator) agree';
}

{
    # Without .cache, a second consumer still finds the Seq consumed.
    my $s = (1..3).map(* + 1);
    $s.List;
    throws-like { $s.List }, X::Seq::Consumed, 'an uncached Seq is still single-use';
}
