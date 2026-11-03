use Test;

# `Any.AT-POS(i) = v` and `Any.ASSIGN-POS(i, v)` on an undefined scalar
# autovivify an Array into the variable, as `$undefined[i] = v` does, and
# `Any.DELETE-POS` of an undefined one answers `Nil` (#16093). Checked against
# rakudo.

plan 9;

{
    my $a;
    is-deeply ($a.AT-POS(2) = 7), 7, 'AT-POS assignment answers the value';
    is-deeply $a, $[Any, Any, 7], '...and vivified an Array with holes before it';
}

{
    my $a;
    is-deeply $a.ASSIGN-POS(1, 5), 5, 'ASSIGN-POS answers the value';
    is-deeply $a, $[Any, 5], '...and vivified an Array';
}

{
    my $a;
    $a.AT-POS(0) = 1;
    $a.AT-POS(1) = 2;
    is-deeply $a, $[1, 2], 'a second AT-POS assignment grows the vivified Array';
}

sub vivify { my $s; $s.AT-POS(0) = "x"; $s }
is-deeply vivify(), $["x"], 'inside a routine';

{
    my $a;
    is-deeply $a.DELETE-POS(0), Nil, 'DELETE-POS of an undefined scalar answers Nil';
    is-deeply $a, Any, '...and vivifies nothing';
}

{
    my @arr = 1, 2;
    @arr.AT-POS(0) = 9;
    is-deeply @arr, [9, 2], 'an Array element is unchanged behaviour';
}
