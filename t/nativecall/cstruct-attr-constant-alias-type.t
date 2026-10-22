use v6;
use Test;
use lib 't/lib';
use NativeCall;
use CArrAliasExport;

plan 7;

# A `constant` alias heading the parameterized type of an attribute
# (`has CA2[uint16] $.kids`) names the aliased type wherever the attribute is
# checked, not only in the frame that declared the alias.
my constant CA2 = CArray;

class P is repr("CStruct") {
    has CA2[uint16] $.kids;
    submethod TWEAK(:$k) { $!kids := CA2[uint16].new(|$k) with $k; }
}
is P.new(:k([1, 2])).kids.^name, 'NativeCall::Types::CArray[uint16]',
    'a CStruct submethod binds an attribute typed through a local alias';

class Q {
    has CA2[uint16] $.kids;
    submethod TWEAK(:$k) { $!kids := CA2[uint16].new(|$k) }
}
is Q.new(:k([1])).kids.^name, 'NativeCall::Types::CArray[uint16]',
    'a plain class does the same';

class U {
    has CA2[uint16] $.kids;
    submethod TWEAK { my $probe = CA2[uint16].new(1); }
}
is U.new.kids.^name, 'NativeCall::Types::CArray[uint16]',
    'an unset attribute reads back the aliased type object';
is U.new.kids.WHAT.raku, 'NativeCall::Types::CArray[uint16]', 'and its .WHAT.raku is the aliased type';

class P2 is repr("CStruct") {
    has CArr[uint16] $.kids;
    submethod TWEAK(:$k) { $!kids := CArr[uint16].new(|$k) with $k; }
}
is P2.new(:k([1, 2])).kids.^name, 'NativeCall::Types::CArray[uint16]',
    'an alias imported by a module works the same way';

class V { has CA2[uint16] $.kids }
is V.new(kids => CA2[uint16].new(3)).kids.elems, 1, 'a passed value passes the type check';

throws-like { V.new(kids => 42) }, X::TypeCheck::Assignment,
    'a value of another type is still rejected';
