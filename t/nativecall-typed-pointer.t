use Test;
use NativeCall;

# A `Pointer[T]` is a pointer that remembers what it points at: `.of` reports
# `T` (`void` when untyped) and `.deref` reads through it. `NativeHelpers::Blob`
# branches on `ptr.of ~~ void`, and `MoarVM::Guts::REPRs` reads a struct with
# `nativecast(Pointer[SomeBody], $addr).deref`.
#
# A `Pointer[T]`-typed *field* is also here: it is one pointer like any other,
# but not recognising the spelling aborted the whole enclosing struct's layout,
# so a struct with a single such field had no layout at all and every field
# access on it failed. DBIish's `MYSQL_BIND` is exactly that shape.

plan 14;

sub calloc(size_t, size_t --> Pointer) is native { * }
sub free(Pointer) is native { * }

class Body is repr('CStruct') {
    has int64 $.alpha  is rw;
    has int64 $.beta   is rw;
}

my $blk = calloc(1, 16);
ok $blk.defined, 'calloc gave us a block to work in';
nativecast(Body, $blk).alpha = 7;

# --- .of ---
# raku spells it `NativeCall::Types::void`; mutsu leaves the NativeCall types
# unqualified throughout (`CArray[uint8]` vs `NativeCall::Types::CArray[uint8]`),
# so match the tail rather than pinning one implementation's namespace.
ok Pointer.new(1).of.^name.ends-with('void'),
                                      'an untyped Pointer points at void';
ok Pointer.new(1).of ~~ void,         'and that is the `void` type itself';
is nativecast(Pointer[Body], $blk).of.^name, 'Body',
                                      'a typed Pointer reports its type';
is nativecast(Pointer[int32], $blk).of.^name, 'int32',
                                      'a typed Pointer over a native scalar';

# --- .deref ---
my $bp = nativecast(Pointer[Body], $blk);
is $bp.Int, $blk.Int,                 'a typed Pointer keeps the address';
is $bp.deref.^name, 'Body',           '.deref on a struct pointer yields the struct';
is $bp.deref.alpha, 7,                'and reads the struct in place';
$bp.deref.beta = 9;
is nativecast(Body, $blk).beta, 9,  'a write through .deref reaches the same memory';

is nativecast(Pointer[int64], $blk).deref, 7,
                                      '.deref on a scalar pointer reads the value';

dies-ok { Pointer.new($blk.Int).deref },
                                      'an untyped Pointer cannot be dereferenced';

free($blk);

# --- a Pointer[T] field is one pointer, and does not break the layout ---
class WithTyped is repr('CStruct') {
    has Pointer[int8] $.err;
    has int32         $.n is rw;
}
is nativesizeof(WithTyped), 16,       'a Pointer[T] field is one pointer, padded';

my $blk2 = calloc(1, 32);
my $w = nativecast(WithTyped, $blk2);
$w.n = 5;
is $w.n, 5,                           'the struct still has a working layout';
is $w.err.Int, 0,                     'and the Pointer[T] field reads as NULL';
free($blk2);
