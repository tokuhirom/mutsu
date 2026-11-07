# A `::T` capture bound to a parameterized type is that type even when the program declares a class
# named `T` (#17076). NativeCall's own `validnctype(Mu ::T)` helper hit it: a program with `class T`
# got a "Not an accepted NativeCall type" warning for every CArray parameter.
use Test;
use NativeCall;
plan 8;

class T { }
role R[::X] { }

sub name-of(Mu ::T) { T.^name }
is name-of(Int), 'Int', 'a plain type (always worked)';
is name-of(Array[Int]), 'Array[Int]', 'a parameterized class';
is name-of(R[Int]), 'R[Int]', 'a role specialization';
is name-of(CArray[int32]), 'NativeCall::Types::CArray[int32]', 'CArray[int32]';
is name-of(Pointer[int32]), 'NativeCall::Types::Pointer[int32]', 'Pointer[int32]';
is name-of(CArray), 'NativeCall::Types::CArray', 'an unparameterized CArray';

my $warnings = 0;
{
    CONTROL { when CX::Warn { $warnings++; .resume } }
    sub takes-array(CArray[int32] $p --> int32) is native('libc.so.6') is symbol('abs') { * }
    my $a = CArray[int32].new: 1, 2;
    sub c-atoi(Str --> int32) is native('libc.so.6') is symbol('atoi') { * }
    is c-atoi("42"), 42, 'a native routine declared next to the same-named class';
}
is $warnings, 0, 'declaring a native routine with a CArray parameter does not warn';
