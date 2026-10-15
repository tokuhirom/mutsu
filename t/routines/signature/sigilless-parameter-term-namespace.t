use Test;

# A sigilless binding (`\t`) and a scalar (`$t`) are different symbols
# (#12441, ADR-12441): neither hides nor overwrites the other.

plan 12;

{
    sub f(\t, $t) { "{t}{$t}" }
    is f(1, 2), '12', 'a sigilless and a scalar parameter of one routine';
    sub g($t, \t) { "{t}{$t}" }
    is g(1, 2), '21', 'the scalar parameter first';
}

{
    my \t = 1;
    is (-> $t { "{t}{$t}" })(2), '12', 'a pointy $t inside a \t scope';
}

{
    my @out;
    for 1 -> \t { my $t = 5; @out.push: $t + t }
    is @out[0], 6, 'a `for` loop\'s \t and a body `my $t`';
}

{
    my @out;
    for 1, 2 -> \k, $k { @out.push: "{k}{$k}" }
    is @out[0], '12', 'multi-parameter loop: \k and $k';
}

{
    my $x = 1;
    my \t := $x;
    my $r;
    { my $t = 5; $x = 3; $r = t }
    is $r, 3, 'a shadowed sigilless binding follows its container';
}

{
    my $t = 'scalar';
    sub h(\t) { t }
    is h('term'), 'term', 'a \t parameter does not read an outer $t';
    is $t, 'scalar', 'and leaves it alone';
}

{
    sub w(\t, $t is copy) { $t = 9; t }
    is w(1, 2), 1, 'writing $t leaves \t alone';
}

{
    my $z = 1;
    sub m(\a, $a) { a = $a }
    m($z, 7);
    is $z, 7, 'assigning through \a writes the caller\'s container';
}

{
    my \t = 5;
    is EVAL('t'), 5, 'EVAL sees a sigilless binding';
    my $t = 6;
    is EVAL('t ~ $t'), '56', 'EVAL tells \t from $t';
}
