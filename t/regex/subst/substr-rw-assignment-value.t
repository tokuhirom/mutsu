use Test;

# An assignment through a `substr-rw` lvalue evaluates to the value stored
# through it (the Proxy's FETCH), not the whole rewritten invocant (#10583).

plan 10;

{
    my $s = 'abc';
    is ($s.substr-rw(0, 1) = 'Y'), 'Y', 'method form evaluates to the assigned value';
    is $s, 'Ybc', '...and still writes the invocant back';
}

{
    my $t = 'abc';
    my $r = ($t.substr-rw(1, 1) = 'Q');
    is $r, 'Q', 'the assigned value is what a later read sees';
    is $t, 'aQc', '...with the write in place';
}

{
    my $v = 'abc';
    is (substr-rw($v, 0, 1) = 'W'), 'W', 'sub form evaluates to the assigned value';
    is $v, 'Wbc', '...and writes the target back';
}

{
    my $w = 'abc';
    my $r = ($w.substr-rw(0, 1) = 5);
    isa-ok $r, Str, 'a non-Str value comes back as the stored Str';
    is $r, '5', '...with its string value';
}

{
    my %h = k => 'abc';
    is (%h<k>.substr-rw(0, 1) = 'Y'), 'Y', 'a hash-element invocant evaluates to the assigned value';
    is %h<k>, 'Ybc', '...and the element is written back';
}
