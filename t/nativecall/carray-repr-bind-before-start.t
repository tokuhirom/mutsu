use Test;
use nqp;

# A store before the start of an `is repr('CArray')` array is dropped:
# upstream NativeCall's `CArray[T].allocate(0)` binds index -1 of an empty
# array and expects it to stay empty (#11203). MoarVM writes before its
# buffer there; mutsu writes nothing.

plan 4;

class CA is repr('CArray') is array_type(int32) { }

my $a := nqp::create(CA);
lives-ok { nqp::bindpos_i($a, -1, 7) }, 'binding index -1 of an empty CArray lives';
is nqp::elems($a), 0, 'and leaves it empty';

nqp::bindpos_i($a, 1, 5);
is nqp::elems($a), 2, 'an ordinary bind still grows it';

# A VMArray keeps MoarVM's own rule: -1 of an empty list is out of bounds.
dies-ok { my $l := nqp::list_i(); nqp::bindpos_i($l, -1, 0) },
    'binding index -1 of an empty nqp list still dies';
