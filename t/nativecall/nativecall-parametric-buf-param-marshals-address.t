use Test;
use NativeCall;

# A `Buf[uint8]`/`Blob[uint8]`-style parametric signature parameter must
# marshal the same way as the bare `Buf`/`Blob` stem does: a `void*` to the
# buffer's raw bytes. `CType::from_type_name` only recognizes the bare stems
# ("Buf", "Blob", "buf8", "blob8"); a bracketed spelling that reached it
# unstripped fell through to the "starts-uppercase => opaque CStruct pointer"
# heuristic, which found no `address` attribute on a plain Buf instance and
# passed NULL to C instead -- silently on a `--> size_t`-returning function,
# or a SEGV on one like `strlen` that dereferences the pointer.
# (todo/tickets/typed-buf-native-interop-holes.md, item 4)

plan 2;

sub strlen(Buf[uint8]) returns size_t is native { * }
my $b = Buf[uint8].new(72, 105, 0); # "Hi\0"
is strlen($b), 2, 'a Buf[uint8] parameter marshals as the buffer address, not NULL';

sub strlen2(Blob[uint8]) returns size_t is native is symbol('strlen') { * }
my $b2 = Blob[uint8].new(87, 111, 0); # "Wo\0"
is strlen2($b2), 2, 'a Blob[uint8] parameter marshals as the buffer address, not NULL';
