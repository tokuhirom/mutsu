use NativeCall;
use Test;

# `HAS T @.x[N] is CArray` where `N` is a named `constant` rather than a
# literal integer (GH #8032, following on from ADR-0090). `cstruct_layout`
# refuses the layout when an embedded shaped array's `declared_shape` is
# `None` (it would otherwise put every later field at a silently wrong
# offset), and a constant-dimension shape used to read as `None` -- the same
# gap `nativecall-has-embedded-struct.t` exercises for a literal dimension.
# `Image::Libexif`'s `ExifData` is exactly this shape, whose `EXIF_IFD_COUNT`
# dimension is a named constant.

plan 1;

constant N = 5;
class S is repr('CStruct') {
    HAS int32 @.a[N] is CArray;
    has int32 $.t;
}
is nativesizeof(S), 24, 'a named-constant CArray shape lays out correctly, matching the literal case';
