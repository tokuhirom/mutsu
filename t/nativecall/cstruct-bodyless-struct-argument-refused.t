use Test;
use NativeCall;

# A struct class with no fields, or with a field NativeCall cannot lay out, has
# no C layout and so no storage. Rakudo refuses to compose such a class at all
# ("Class E has no attributes, which is illegal with the CStruct
# representation"); mutsu composes it, so an object of one that is handed to a
# native routine must be refused with a catchable error. It used to reach the
# callee as NULL and crash the process when the callee dereferenced it (#11753).

plan 4;

class Empty is repr('CStruct') { }
class Unlaid is repr('CStruct') { has Int $.x }
sub memchr-empty(Empty, int32, size_t --> Pointer) is native('c', v6) is symbol('memchr') { * }
sub memchr-unlaid(Unlaid, int32, size_t --> Pointer) is native('c', v6) is symbol('memchr') { * }

throws-like { memchr-empty(Empty.new, 1, 4) }, X::AdHoc,
    message => /'Native call expected argument 1 with CStruct representation'/,
    'a struct with no fields is refused rather than passed as NULL';
throws-like { memchr-unlaid(Unlaid.new(x => 3), 1, 4) }, X::AdHoc,
    message => /'Native call expected argument 1 with CStruct representation'/,
    'so is one with a field that has no C layout';

# A type object is still a NULL pointer, as in Rakudo, and a laid-out struct is
# still passed.
class Fine is repr('CStruct') { has int32 $.a }
sub memchr-fine(Fine, int32, size_t --> Pointer) is native('c', v6) is symbol('memchr') { * }
nok memchr-fine(Fine, 1, 0).defined, 'a type object is a NULL pointer';
ok memchr-fine(Fine.new(a => 1), 1, 4).defined, 'a struct with a layout is passed';
