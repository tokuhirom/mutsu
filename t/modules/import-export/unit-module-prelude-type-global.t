use v6;
use lib 't/lib';
use Test;

plan 8;

use UnitModPrelude;
use NativeCall;

# The builtin preludes (NativeCall's `Pointer`/`void`, the `Rational` role) are
# spliced into whichever compunit references them. A `unit module` switches the
# runtime package for the whole unit, so an unqualified prelude declaration used
# to register under the module's package (`UnitModPrelude::Pointer`) instead of
# globally -- a distinct type that could not be parameterized.

isa-ok prelude-pointer(), Pointer,
    'a module-internal `Pointer` is the same type as the caller-visible one';

ok prelude-pointer() === Pointer,
    'the module-internal `Pointer` is the very same type object';

lives-ok { prelude-pointer-of() },
    'parameterizing `Pointer` inside a `unit module` does not throw';

# Not `is ... , 'Pointer[uint8]'`: raku names the type
# `NativeCall::Types::Pointer[uint8]` and mutsu's prelude class is plain
# `Pointer`, a separate naming gap tracked in
# todo/tickets/nativecall-pointer-short-name.md. What matters here is that the
# base is the *global* `Pointer`, not the host module's package.
ok prelude-pointer-of().^name.ends-with('Pointer[uint8]'),
    'the parameterized type carries the global base name';

nok prelude-pointer-of().^name.contains('UnitModPrelude'),
    'the parameterized type is not qualified with the declaring module';

ok prelude-void() === void,
    'the prelude `void` type is global too';

# Same story for the `Rational` role prelude.
ok UnitRat ~~ Rational,
    'a class in a `unit module` composes the global `Rational` role';

is UnitRat.new(3, 4).nude, (3, 4),
    'the composed role behaves like the builtin one';
