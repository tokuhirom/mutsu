use Test;

# `for @b` aliases `$_` (or a `<->`/`is rw` loop param) to each element, so a
# raw (sigilless) or `is rw` parameter bound to it is that element's container
# and an assignment through it writes the array (#11447).

plan 6;

sub g(\s) { s = "X" }
sub r($s is rw) { $s = "R" }

{
    my @b = <a b>;
    for @b { g($_) }
    is-deeply @b, [<X X>], 'g($_) writes the element through a sigilless param';
}

{
    my @b = <a b>;
    for @b { .&g }
    is-deeply @b, [<X X>], '.&g writes the element through a sigilless param';
}

{
    my @b = <a b>;
    for @b { r($_) }
    is-deeply @b, [<R R>], 'r($_) writes the element through an `is rw` param';
}

{
    my @b = <a b>;
    for @b <-> $x { g($x) }
    is-deeply @b, [<X X>], 'a `<->` loop param passed to a sigilless param writes';
}

{
    my @b = <a b>;
    for @b -> $x is rw { r($x) }
    is-deeply @b, [<R R>], 'an `is rw` loop param passed to an `is rw` param writes';
}

{
    my @b = <a b>;
    for @b { .&r }
    is-deeply @b, [<R R>], '.&r writes the element through an `is rw` param';
}
