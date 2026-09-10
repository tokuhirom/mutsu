use Test;
use NativeCall;

# The four `nqp::` ops `NativeHelpers::Blob` / `NativeHelpers::Pointer` are
# written in terms of. They are what makes pointer arithmetic and buffer
# allocation expressible from Raku:
#
#     my int $a = nqp::unbox_i(nqp::decont(self)) + $off * nativesizeof(type);
#     nqp::box_i($a, Pointer[type]);           # NativeHelpers::Pointer.add
#     nqp::setelems(b, nqp::unbox_i($elems.Int));   # blob-allocate

plan 9;

# unbox_i / box_i round-trip through a Pointer's address.
my $p = Pointer.new(0x1000);
is nqp::unbox_i($p), 0x1000, 'nqp::unbox_i yields a Pointer address';
is nqp::box_i(0x2000, Pointer).Int, 0x2000, 'nqp::box_i builds a Pointer at an address';
is nqp::unbox_i(nqp::box_i(0x3000, Pointer)), 0x3000, 'and the two round-trip';

# A zero address is a *defined* Pointer -- `Pointer.new(0)` is legitimate,
# unlike a native call's NULL return (which is the class's type object).
ok nqp::box_i(0, Pointer).defined, 'a NULL Pointer built this way is still defined';

# A typed target remembers what it points at, so `.of` reports it.
is nqp::box_i(0x4000, Pointer[int32]).of.^name, 'int32',
    'nqp::box_i keeps the Pointer parameter';

# decont strips the container off a `$`-variable.
my $n = 7;
is nqp::unbox_i(nqp::decont($n)), 7, 'nqp::decont then unbox_i reads through a Scalar';

# setelems resizes a buffer, the new elements zero. `blob-allocate` is
# `blob.new` followed by exactly this.
my $b = Buf.new(1, 2, 3);
nqp::setelems($b, 5);
is $b.bytes, 5, 'nqp::setelems grows a Buf';
is $b[4], 0, 'and the new elements are zero';
nqp::setelems($b, 2);
is $b.bytes, 2, 'and it shrinks too';
