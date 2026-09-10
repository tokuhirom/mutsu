use Test;

# `^add_method` on a **qualified spelling** of an already-registered class must
# add the method to that class, not to a fresh stub nobody consults.
# `NativeHelpers::Pointer` builds all of NativeCall's pointer arithmetic this way
# (`NativeCall::Types::Pointer.^add_method('add', …)`), while the prelude
# registers `Pointer` under its short name and tags every handle with it — so the
# methods landed on an unreachable stub, `.add` was "no such method", and
# `.succ`/`.pred` fell through to the numeric successor (advancing by one *byte*).
#
# And an added method's invocant must not be counted as a parameter: `method
# (Pointer:D: Int $off)` bound `$off` to nothing and died with "Variable 'off' is
# not declared".

use NativeCall;

plan 11;

# --- The invocant is not a parameter ---
class Plain {}
Plain.^add_method('two', method ($a, $b) { "$a-$b" });
is Plain.new.two(1, 2), '1-2', 'positional parameters of an added method bind';

Plain.^add_method('typed', method (Plain:D: Int $off) { $off * 3 });
is Plain.new.typed(4), 12, 'an explicit invocant is not counted as a parameter';

Plain.^add_method('named-invocant', method (Plain:D $self: Str $s) { $s.uc });
is Plain.new.named-invocant('hi'), 'HI', 'a named explicit invocant is dropped too';

Plain.^add_method('none', method () { 'nullary' });
is Plain.new.none, 'nullary', 'a nullary added method still works';

# --- A qualified spelling reaches the registered class ---
class Short {}
Short.^add_method('here', method () { 'short' });
is Short.new.here, 'short', 'the unqualified spelling works as before';

# `Any` is registered under its short name; add through a qualified spelling and
# the instance — tagged with the short name — must still find it.
NativeCall::Types::Pointer.^add_method('mutsu-probe', method () { 'found' });
my $p = Pointer.new(1024);
is $p.mutsu-probe, 'found', 'a qualified spelling adds to the registered class';

# --- Pointer arithmetic, the reason the two bugs above mattered ---
use NativeHelpers::Pointer;

my $a = CArray[uint16].new(10, 20, 30, 40);
my $base = nativecast(Pointer[uint16], $a);
is $base.deref, 10,                     'the cast pointer reads element 0';
is $base.succ.deref, 20,                '.succ advances by one *element*, not one byte';
is $base.add(2).deref, 30,              '.add(n) advances by n elements';
is $base.add(3).pred.deref, 30,         '.pred goes back one element';

# `isa-ok $p.succ, Pointer[uint16]` — a typed pointer keeps its parameterisation
# in an `of` attribute rather than in its class name, so the type check has to
# read it from there.
ok $base.succ ~~ Pointer[uint16],       'an arithmetic result matches Pointer[T]';
