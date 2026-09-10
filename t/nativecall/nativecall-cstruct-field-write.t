use Test;
use NativeCall;

# The write half of `is repr('CStruct')` field access. A CStruct handle keeps no
# Raku attributes — the C struct its `address` points at is the only storage it
# has — so an assignment through it must write native memory. Before this the
# write fell through to the ordinary attribute path, reported success, and went
# nowhere: `$s.a = 42` read back as 0.
#
# `.^array_type` is here too: `NativeHelpers::Blob` asks every container it is
# handed for its element type and feeds the answer to `nativesizeof` and
# `nativecast(Pointer[T], ...)`.

plan 17;

sub calloc(size_t, size_t --> Pointer) is native { * }
sub free(Pointer) is native { * }

class Rec is repr('CStruct') {
    has int32  $.i32 is rw;
    has int64  $.i64 is rw;
    has num64  $.n64 is rw;
    has uint8  $.u8  is rw;
    # An address-holding field is declared as an integer, not as `Pointer`:
    # Rakudo refuses to assign through a `Pointer`-typed accessor ("Cannot
    # modify an immutable Pointer type object"), which is why DBIish's
    # `MYSQL_BIND` declares `has intptr $.buffer is rw` with the `Pointer`
    # version commented out.
    has uint64 $.ptr is rw;
}

my $block = calloc(1, 64);
ok $block.defined, 'calloc gave us a block to work in';
my $r = nativecast(Rec, $block);

# --- scalar fields ---
is $r.i32, 0,                   'calloc zeroed the block, so i32 starts at 0';
$r.i32 = 42;
is $r.i32, 42,                  'an int32 field write reaches C memory';
$r.i32 = -7;
is $r.i32, -7,                  'an int32 field holds a negative value';

$r.i64 = 4294967296;            # 2**32, so a 32-bit write would lose it
is $r.i64, 4294967296,          'an int64 field write keeps the high word';

$r.n64 = 1.5e0;
is $r.n64, 1.5e0,               'a num64 field write round-trips as a float';

$r.u8 = 200;
is $r.u8, 200,                  'a uint8 field write round-trips';
is $r.i32, -7,                  'writing one field leaves its neighbour alone';

# --- an address stored in an integer field, the way real bindings do it ---
my $other = calloc(1, 8);
$r.ptr = $other.Int;
is $r.ptr, $other.Int,          'an address written into an integer field round-trips';
$r.ptr = Pointer.new(0xdead000).Int;
is $r.ptr, 0xdead000,           'a freshly built Pointer address round-trips';

# A second handle onto the same block sees the writes: there is one struct, not
# a per-handle copy.
my $alias = nativecast(Rec, $block);
is $alias.i64, 4294967296,      'a second handle onto the same address sees the write';

free($other);
free($block);

# --- .^array_type ---
is Buf.new(1, 2, 3).^array_type.^name, 'uint8',
                                'a Buf is an array of uint8';
is Buf[uint64].new(1, 2).^array_type.^name, 'uint64',
                                'a parameterised Buf reports its element type';
is 'ab'.encode('utf8').^array_type.^name, 'uint8',
                                'utf8 is an array of uint8';
is CArray[int32].new.^array_type.^name, 'int32',
                                'a CArray reports its element type';
my array[uint8] $a .= new(1, 2, 3);
is $a.^array_type.^name, 'uint8',
                                'a native array reports its element type';
is nativesizeof(CArray[int32].new.^array_type), 4,
                                'the reported element type feeds nativesizeof';
