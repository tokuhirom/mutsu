use Test;

# A bare block binds its implicit `$_` raw to its argument, so a write made to
# the caller's variable while the block runs is visible through `$_` even when
# the block only reads `$_` (#11298).

plan 12;

{
    my $x = 1;
    my $seen;
    my &b = { $x = 5; $seen = $_ };
    b($x);
    is $seen, 5, 'a named block sees its own write to the argument variable';
}

{
    my $y = 1;
    my $seen;
    { $y = 5; $seen = $_ }($y);
    is $seen, 5, 'an immediately called block sees its write through $_';
    is $y, 5, 'the write reaches the caller';
    $y = 3;
    is $y, 3, 'the caller variable is still assignable afterwards';
}

{
    my $y = 1;
    my @seen;
    { @seen.push: $_; $y = 8; @seen.push: $_ }($y);
    is @seen.join(','), '1,8', '$_ reads the variable before and after the write';
}

{
    sub f { my $z = 1; my @in; for ^2 { { $z++; @in.push: $_ }($z) }; (@in.join(','), $z) }
    is f().join('/'), '2,3/3', 'repeated calls in a routine loop';
}

{
    my $q = 1;
    my @r = (^3).map({ { $q = $q + 10; $_ }($q) });
    is @r.join(','), '11,21,31', 'inside a map block';
    is $q, 31, 'the map block writes reach the outer variable';
}

{
    sub setv { $*V = 7 }
    sub m(&block) { my $*V = 1; block $*V; $*V }
    my $seen;
    is m({ $*V = 7; $seen = $_ }), 7, 'a block writing a dynamic argument variable';
    is $seen, 7, '... sees the write through $_';
    m({ setv(); $seen = $_ });
    is $seen, 7, 'a callee writing the dynamic variable is seen through $_';
}

{
    sub own { my $*V = 100; $*V = 200 }
    sub m2(&block) { my $*V = 1; block $*V; $*V }
    my $seen;
    m2({ own(); $seen = $_ });
    is $seen, 1, 'a callee writing its own dynamic of the same name is not seen';
}
