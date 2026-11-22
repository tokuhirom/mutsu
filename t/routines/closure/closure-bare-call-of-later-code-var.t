use Test;
use nqp;

# A bare `d` (no argument list) naming a `my &d` declared later in the same
# scope calls it, from a closure created before the declaration too: the
# closure captures the declaring frame's `&d` (#18248; CBOR::Simple's
# mutually recursive decoders).

plan 5;

sub t() {
    my &list = { (^2).map(&d).List };
    my &one  = { my $x = d; $x };
    my &d    = { 7 };
    (list(), one())
}
is-deeply t(), ((7, 7), 7), 'a bare call and a &d reference before the declaration';

sub t2() {
    my &m = { nqp::if(1, d, 2) };
    my &d = { 8 };
    m()
}
is t2(), 8, 'a bare call as an nqp::if argument';

sub counter() {
    my $n = 0;
    my &next = { bump; $n };
    my &bump = { $n++ };
    next() for ^3;
    $n
}
is counter(), 3, 'a bare call of a later &bump that writes the outer $n';

{
    my &d = { 'outer' };
    sub call-it() { d }
    is call-it(), 'outer', 'a bare call of an enclosing scope\'s &d';
}

my \d = 'term';
is d, 'term', 'a sigilless d is still the term, not a call';
