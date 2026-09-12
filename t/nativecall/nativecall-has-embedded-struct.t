use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;
use NativeCall;

# NativeCall's `HAS` declarator: an attribute stored **by value** inside the
# enclosing `is repr('CStruct')` class, instead of as a pointer to it.
#
# Before this, `HAS gsl_vector $.vector` parsed as a call to a function named
# `HAS` taking `$.vector`, so the line died with "Variable $.vector used where
# no 'self' is available" (or "Unknown function: HAS", depending on how the
# parse recovered) and the whole module failed to load. That blocked up to 44
# ecosystem distributions -- `Math::Libgsl::*`, `Raylib::Bindings`,
# `Image::Libexif`, `Net::Ethereum`, ... -- see GH #7991.
#
# The sizes asserted here are C's, and were measured against rakudo.

plan 23;

sub calloc(size_t, size_t --> Pointer) is native { * }
sub free(Pointer) is native { * }

class Point is repr('CStruct') {
    has int32 $.x is rw;
    has int32 $.y is rw;
}

class Line is repr('CStruct') {
    HAS Point $.from;
    HAS Point $.to;
    has int32 $.width is rw;
}

# A flat view of the same bytes, so the test can prove *where* each member
# landed rather than only that it round-trips.
class Flat is repr('CStruct') {
    has int32 $.a is rw;
    has int32 $.b is rw;
    has int32 $.c is rw;
    has int32 $.d is rw;
    has int32 $.e is rw;
}

is nativesizeof(Point), 8,      'the embedded class is two int32s';
is nativesizeof(Line), 20,      'HAS members are inlined: 8 + 8 + 4, not two pointers';

my $block = calloc(1, 64);
ok $block.defined,              'calloc gave us a block to work in';
my $line = nativecast(Line, $block);
my $flat = nativecast(Flat, $block);

is $line.from.^name, 'Point',   'a HAS member reads back as its declared class';
is $line.from.x, 0,             'calloc zeroed the block, so the member starts at 0';

$line.from.x = 1;
$line.from.y = 2;
$line.to.x   = 3;
$line.to.y   = 4;
$line.width  = 5;

is $flat.a, 1,                  'from.x is the first word of the enclosing struct';
is $flat.b, 2,                  'from.y is the second';
is $flat.c, 3,                  'to.x follows the whole first member, not a pointer';
is $flat.d, 4,                  'to.y is the fourth';
is $flat.e, 5,                  'the scalar tail comes after both members';

is $line.to.y, 4,               'a HAS member reads back what was written through it';
free($block);

# An embedded member is storage, not a reference: two handles onto the same
# block see one struct.
{
    my $blk = calloc(1, 64);
    my $a = nativecast(Line, $blk);
    my $b = nativecast(Line, $blk);
    $a.from.x = 99;
    is $b.from.x, 99,           'there is one struct, not a copy per handle';
    free($blk);
}

# `HAS Type $!x` -- the private-twigil spelling -- is the same declaration and
# takes up the same storage, which the public tail behind it proves. Reading
# it back as `$!x` from inside a method used to be a *separate* gap (GH
# #8030): mutsu resolved `$!a` on a CStruct handle through the instance's
# (empty) Raku attributes instead of through native memory, so even a plain
# `has int32 $!a` read as Nil there. Fixed alongside this test's restored
# assertion.
{
    class Priv is repr('CStruct') {
        HAS Point $!p;
        has int32 $.tail is rw;
        method px() { $!p.x }
    }
    is nativesizeof(Priv), 12,  'a private HAS member is laid out the same way';
    my $blk = calloc(1, 32);
    my $p = nativecast(Priv, $blk);
    $p.tail = 17;
    is nativecast(Flat, $blk).c, 17,
                                'the tail sits past the private member, not past a pointer';
    nativecast(Point, $blk).x = 42;
    is $p.px, 42,               'a private $!x read reaches native memory, not Nil';
    free($blk);
}

# Alignment: an embedded member pads to its strictest field, and the struct
# itself rounds up so an array of it stays aligned.
{
    class Wide is repr('CStruct') {
        has int32 $.n is rw;
        has num64 $.d is rw;
    }
    class Holder is repr('CStruct') {
        has int8  $.head is rw;
        HAS Wide  $.wide;
        has int8  $.tail is rw;
    }
    is nativesizeof(Wide), 16,   'the member pads to its num64';
    is nativesizeof(Holder), 32, 'the member aligns to 8 inside its holder';
}

# `HAS` on a native scalar inlines nothing -- a plain `has` already stores it
# that way -- so rakudo accepts the declaration, warns, and carries on. It is a
# warning and not an error: the declaration must still lay out normally.
is_run ｢use NativeCall;
        class S is repr('CStruct') { HAS int32 $.a; has int32 $.b }
        print nativesizeof(S)｣,
    %(:out<8>, :err{ .contains: 'Useless use of HAS scope on int32 typed attribute' }),
    'HAS on a native scalar warns and lays out as an ordinary field';

# ... and only on a *scalar*: an inline array is exactly what HAS is for.
is_run ｢use NativeCall;
        class S is repr('CStruct') { HAS num32 @.mat[4] is CArray; has int32 $.t }
        print nativesizeof(S)｣,
    %(:out<20>, :err{ !.contains('Useless use of HAS scope') }),
    'HAS on a shaped array is an inline array, and warns about nothing';

# An inline array occupies all N elements, so the tail that follows it is at
# the right offset and the elements are reachable.
{
    class Mat is repr('CStruct') {
        HAS int32 @.m[4] is CArray;
        has int32 $.t is rw;
    }
    is nativesizeof(Mat), 20,   'four inline int32s plus the tail';
    my $blk = calloc(1, 64);
    my $mat = nativecast(Mat, $blk);
    $mat.t = 42;
    # Rakudo spells it `NativeCall::Types::CArray[int32]`; both name the same
    # parameterisation, and what matters is that it is a CArray and not a bare
    # `Pointer` (which cannot be indexed).
    ok $mat.m.^name.ends-with('CArray[int32]'), 'an inline array reads back as a CArray';
    # Direct form (GH #8031): assigning into an index of the accessor's
    # result, with no intermediate `my $m = $mat.m;` binding, writes into
    # the same native memory.
    $mat.m[2] = 7;
    is nativecast(Flat, $blk).c, 7, 'element 2 is the third word of the struct';
    is $mat.t, 42,                  'the tail sits past the whole array';
    free($blk);
}
