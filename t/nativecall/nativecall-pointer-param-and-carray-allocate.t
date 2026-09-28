use Test;
use NativeCall;

# `Pointer[T]` as a parameter/variable type constraint used to fail at compile
# time with an X::NotParametric "Pointer cannot be parameterized", even though
# `my Pointer[T] $x` worked fine. `Pointer` is spliced into every NativeCall
# program as a genuine `class GLOBAL::Pointer` prelude
# (`run::NATIVECALL_POINTER_PRELUDE`), so the compile-time signature pre-pass
# collected it into `declared_classes` and mis-flagged its own `[T]` as
# parameterizing a non-parametric class (#9836).
#
# `CArray[T].allocate(n)` was also entirely missing: `CArray.new` followed by
# manual out-of-range assignment (the documented pre-2018.05 workaround) was
# the only way to pre-size a buffer.

plan 10;

sub g(Pointer[uint16] $p) { $p.^name }
is g(Pointer[uint16].new), 'NativeCall::Types::Pointer[uint16]',
    'a Pointer[T] parameter type constraint compiles and dispatches';

my Pointer[uint16] $decl .= new;
is $decl.^name, 'NativeCall::Types::Pointer[uint16]',
    'a Pointer[T] variable type constraint still works (regression guard)';

# --- CArray[T].allocate on a native numeric element type ---
my $a = CArray[int32].allocate(3);
is $a.elems, 3,                        'CArray[int32].allocate(3) has 3 elements';
is $a[0], 0,                           'element 0 is zero-filled';
is $a[2], 0,                           'element 2 is zero-filled';
$a[1] = 42;
is $a[1], 42,                          'an allocated element is still assignable';

is CArray[uint8].allocate(0).elems, 0, 'allocate(0) is an empty CArray';

# --- CArray[T].allocate on a reference element type ---
my $s = CArray[Str].allocate(2);
is $s.elems, 2,                        'CArray[Str].allocate(2) has 2 elements';
isa-ok $s[0].WHAT, Str,                'an unset Str element is the Str type object';
nok $s[0].defined,                     'and it is undefined';
