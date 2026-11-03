use v6;
use Test;

# A named sub an EVAL'd unit declares sees what the unit's own `use` imported
# (an exported constant that is a type: `Pointer` from NativeCall), as an
# anonymous sub or a block of the unit does. A `use` inside a block of the unit
# stays that block's.

plan 6;

is EVAL('use NativeCall; sub f() { Pointer }; f().^name'), 'NativeCall::Types::Pointer',
    'a named sub reads the imported type';
is EVAL('use NativeCall; sub f() { Pointer[int8] }; f().^name'),
    'NativeCall::Types::Pointer[int8]', 'and parameterizes it';
is EVAL('use NativeCall; sub f() { CArray[int8].new }; f().^name'),
    'NativeCall::Types::CArray[int8]', 'a second imported type';
is EVAL('use NativeCall; my $s = sub { Pointer }; $s().^name'), 'NativeCall::Types::Pointer',
    'an anonymous sub (already worked)';
is EVAL('{ use NativeCall; 1 }; sub f() { try { Pointer } // "hidden" }; f() ~~ Str ?? "hidden" !! "seen"'),
    'hidden', 'a use inside a block of the unit is not seen by a sub outside it';

# The same through `EVAL` of a RakuAST tree.
is EVAL(Q{use NativeCall; sub f() { Pointer[int8] }; f().^name}.AST),
    'NativeCall::Types::Pointer[int8]', 'EVAL of the AST: a named sub reads the imported type';
