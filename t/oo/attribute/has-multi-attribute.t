use Test;
use NativeCall;

plan 7;

# A type constraint before a parenthesised attribute list applies to every
# declaration in the list, rather than leaving the list to the expression
# parser (which evaluates `$.x` without a `self`).
class Point {
    has Int ($.x, $.y);
}

my $point = Point.new(x => 1, y => 2);
is $point.x, 1, 'the first typed attribute is declared';
is $point.y, 2, 'the second typed attribute is declared';
is Point.^attributes.elems, 2, 'the typed list creates two attributes';

# NativeCall uses the same declaration form for scalar CStruct fields. This
# pins the parser path without assuming that a CStruct is allocatable beyond
# the existing scalar-field support.
class NativePoint is repr('CStruct') {
    has int32 ($.x, $.y, $.z);
}

my $native = NativePoint.new(x => 3, y => 4, z => 5);
is $native.x, 3, 'the first native field is declared';
is $native.y, 4, 'the second native field is declared';
is $native.z, 5, 'the third native field is declared';
is nativesizeof(NativePoint), 12, 'all native fields contribute to the CStruct layout';

