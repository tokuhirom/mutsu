use Test;

# A `CArray[T]` over a native numeric `T` keeps its elements in contiguous C
# memory (ADR-0015 P3), so it can answer `.REPR`/`.WHERE` honestly, hand a native
# call a pointer into its own storage, and be reinterpreted by `nativecast`.
#
# The pointer C is given is the array's own memory: a callee that writes into it
# needs no sync point, and one that keeps the pointer keeps seeing live data. The
# per-call copy this replaced could only reflect writes made *during* the call.

use NativeCall;

plan 25;

# --- The representation is honest, and it has a real address ---
my $u = CArray[uint16].new(10, 20, 30);
is $u.REPR, 'CArray',                'a native-backed CArray reports its REPR';
ok $u.WHERE > 0,                     'and has a real body address';
isnt $u.WHERE, $u.WHERE + 1,         'sanity: WHERE is a number';
is $u.WHERE, $u.WHERE,               'the body address is stable';

# Element types that are *references* keep the boxed representation, and go on
# under-reporting `P6opaque` — an honest name is a promise that a body exists.
is CArray[Str].new('a').REPR, 'P6opaque',
    'a reference-element CArray keeps under-reporting its REPR';

# --- Elements round-trip at the declared width, signedness and kind ---
is CArray[int8].new(-1)[0], -1,      'signed elements read back signed';
is CArray[uint8].new(-1)[0], 255,    'unsigned elements read back unsigned';
is CArray[int32].new(-70000)[0], -70000, 'a wide signed element keeps its range';
is CArray[num64].new(1.5e0, -2.25e0)[1], -2.25e0, 'num64 elements are Nums';
is CArray[num32].new(0.5e0)[0], 0.5e0, 'num32 elements are Nums';
is CArray[num64].new(1.5e0).of, num64, '.of is the element type';

# Growing on element assignment zero-fills, as C memory does — the boxed
# representation left `Any` holes.
my $g = CArray[uint8].new;
$g[3] = 7;
is $g.elems, 4,                      'element assignment grows the array';
is-deeply $g.list, (0, 0, 0, 7),     'the gap is zeros, not Any holes';

# --- The pointer handed out is the array's own storage ---
my $p = nativecast(Pointer[uint16], $u);
is $p.deref, 10,                     'nativecast(Pointer[T], $carray) points at element 0';
is $p.of, uint16,                    'and remembers the element type it was cast to';

# A native call is handed that same pointer, so a write C makes is a write to
# the Raku object with nothing copied back.
sub c_memcpy(CArray[uint8] $dst, CArray[uint8] $src, size_t $n) returns Pointer
    is native('c') is symbol('memcpy') { * }
my $dst = CArray[uint8].new(0, 0, 0, 0);
c_memcpy($dst, CArray[uint8].new(9, 8, 7, 6), 4);
is-deeply $dst.list, (9, 8, 7, 6),   'a callee writes straight into the array';

# The interesting case the copy could not serve: C keeps the pointer and writes
# through it *later*, with no call boundary in between. `memcpy` into an address
# taken before the write stands in for `NativeHelpers::Blob`'s managed
# `carray-from-blob`.
sub c_memcpy_p(Pointer $dst, CArray[uint8] $src, size_t $n) returns Pointer
    is native('c') is symbol('memcpy') { * }
my $late = CArray[uint8].new(0, 0, 0);
my $addr = nativecast(Pointer[uint8], $late);
c_memcpy_p($addr, CArray[uint8].new(1, 2, 3), 3);
is-deeply $late.list, (1, 2, 3),     'a write through a retained pointer is visible';

# An ordinary Raku-side write does not move the storage, so a pointer taken
# earlier stays valid (ADR-0015 contract 3).
$late[0] = 42;
is nativecast(Pointer[uint8], $late).Int, $addr.Int,
    'a same-size write keeps the storage in place';
is $addr.deref, 42,                  'and the old pointer sees the new value';

# --- Positional behaviour is unchanged by the representation ---
is $u.elems, 3,                      '.elems counts elements';
is-deeply $u.list, (10, 20, 30),     '.list is a List of the elements';
is $u.end, 2,                        '.end is the last index';
is-deeply (gather for @$u { take $_ }), (10, 20, 30), 'it iterates';
ok $u ~~ CArray[uint16],             'it smartmatches its parametric type';
nok $u ~~ CArray[uint32],            'and not a different element type';
