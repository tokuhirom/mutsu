use v6;
use Test;
use NativeCall;

# An untyped `CArray.new` is a `CArray`, not a plain `Array` (#12085): it
# smartmatches `CArray`, binds a `CArray` parameter and reports the NativeCall
# type's name, exactly like the typed spellings.

plan 15;

my $a = CArray.new;
is $a.^name, 'NativeCall::Types::CArray', '.^name of an untyped CArray is the qualified type';
ok $a ~~ CArray, 'an untyped CArray smartmatches CArray';
ok $a ~~ CArray:D, 'and CArray:D';
nok [1, 2] ~~ CArray, 'a plain Array still is not a CArray';
is $a.elems, 0, 'a fresh untyped CArray is empty';

sub takes-carray(CArray $x) { 1 }
sub takes-defined(CArray:D $x) { 1 }
is takes-carray($a), 1, 'an untyped CArray binds a CArray parameter';
is takes-defined($a), 1, 'and a CArray:D parameter';
is takes-carray(CArray[int32].new(1, 2)), 1, 'a typed CArray still binds';

# A native routine takes it too: it has no elements and no element type, so
# C sees its empty storage as NULL (`free(NULL)` is a no-op).
sub c_free(CArray) is native('c', v6) is symbol('free') { * }
lives-ok { c_free(CArray.new) }, 'an untyped CArray is a NULL argument to a native routine';

# The reference-element spellings carry the same qualified name, as in Rakudo.
is CArray[Str].new('a').^name, 'NativeCall::Types::CArray[Str]',
    'a CArray[Str] instance reports the qualified name';
is CArray[int32].new(1).^name, 'NativeCall::Types::CArray[int32]',
    'a CArray[int32] instance reports the qualified name';
is CArray.new.WHAT.^name, 'NativeCall::Types::CArray', '.WHAT.^name agrees';

# A plain Array keeps its own name and is not taken for a CArray.
is [1, 2].^name, 'Array', 'a plain Array keeps its name';
is Array[Int].new(1).^name, 'Array[Int]', 'a typed Array keeps its name';
ok CArray.new !~~ Array[Int], 'an untyped CArray is not an Array[Int]';

# vim: expandtab shiftwidth=4
