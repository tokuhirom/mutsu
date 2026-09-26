use Test;
use NativeCall;

plan 10;

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

# A parenthesized list attribute may carry a per-attribute `= default`
# (PDF::Content::Ops's `has Numeric ($.tf-x = 0.0, $.tf-y = 0.0) is rw;`).
# Rakudo parses but ignores it: an unset attribute reads back as the
# declared type's own type object, not the written default, and `is rw`
# still applies to every attribute in the list.
class Flow {
    has Numeric ($.tf-x = 0.0, $.tf-y = 0.0) is rw;
    method bump { $!tf-x = 1 }
}
my $flow = Flow.new;
is $flow.tf-x, Numeric, 'an in-list default is parsed but not applied';
$flow.bump;
is $flow.tf-x, 1, 'the private name still writes through';
$flow.tf-y = 5;
is $flow.tf-y, 5, 'is rw on the list still applies per attribute';

