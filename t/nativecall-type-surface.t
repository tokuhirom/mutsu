use Test;
use lib $?FILE.IO.parent.add('lib-nativecall-surface').Str;
use NativeCall;
use NCSurface;

# `use NativeCall` exports the whole `NativeCall::Types` set. Three of them were
# simply not declared -- `bool`, `ssize_t` and `OpaquePointer` -- so naming one
# as a term degraded to the `Str` an undeclared bareword becomes. `void` was
# declared but gated on the source ALSO naming `Pointer`, so this file (which
# does not) would not have seen it either.

plan 24;

# --- bool: C's `_Bool`. One byte, and *signed* -- Rakudo answers -1 for
# `my bool $x = -1` and 44 for `= 300`, i.e. exactly `int8`. It is an integer
# type there too (a native `bool` return boxes to Int, not to Bool).
is bool.^name, 'bool', 'bool is a declared type object, not a bareword Str';
is nativesizeof(bool), 1, 'nativesizeof(bool) is 1';
my bool $b = 1;
is $b, 1, 'a bool scalar holds its value';
my bool $neg = -1;
is $neg, -1, 'bool is signed: -1 stays -1';
my bool $wrap = 300;
is $wrap, 44, 'bool is one byte: 300 wraps to 44';

my $ba = CArray[bool].new;
$ba[0] = -1;
is $ba[0], -1, 'CArray[bool] round-trips a signed byte';

# --- ssize_t: the signed counterpart of size_t, 64-bit on every platform
# mutsu targets.
is ssize_t.^name, 'ssize_t', 'ssize_t is a declared type object';
is nativesizeof(ssize_t), 8, 'nativesizeof(ssize_t) is 8';
my ssize_t $s = -4096;
is $s, -4096, 'an ssize_t scalar is signed';
is nativesizeof(size_t), nativesizeof(ssize_t), 'size_t and ssize_t are the same width';

# --- OpaquePointer: NativeCall's historical spelling of Pointer, and an ALIAS
# rather than a subclass, so identity holds.
ok OpaquePointer === Pointer, 'OpaquePointer is Pointer itself, not a subclass';
my $p = OpaquePointer.new(0);
ok $p ~~ Pointer, 'an OpaquePointer-constructed object is a Pointer';
is $p.Int, 0, 'and carries its address';
ok Pointer.new(7) ~~ OpaquePointer, 'a Pointer smartmatches OpaquePointer';

# --- void: reachable without the file also mentioning `Pointer`.
is void.^name, 'void', 'void is a declared type object';
nok void.defined, 'void is a type object';

# All four are usable where a native type is expected: in a signature, as an
# attribute, and as a CStruct field type.
class Rec is repr('CStruct') {
    has bool $.flag;
    has ssize_t $.offset;
    has OpaquePointer $.data;
}
is nativesizeof(Rec), 24, 'a CStruct lays bool out as one byte (padded to the 8-byte members)';

sub takes-them(bool $f, ssize_t $o --> ssize_t) { $f ?? $o !! -$o }
is takes-them(1, 5), 5, 'bool and ssize_t parameters bind';
is takes-them(0, 5), -5, 'and the ssize_t return type is signed';

# `Bool` unboxes to 1/0 in a native integer slot -- which is how `True` reaches
# a C `_Bool` parameter. Before this it went through the numeric catch-all and
# every Bool argument arrived as 0.
sub c_abs(int32 $n --> int32) is native('c') is symbol('abs') { * }
is c_abs(True), 1, 'a Bool argument unboxes to 1 at the C boundary';

# A `unit module` is where prelude scoping breaks: the runtime package switch is
# emitted at the top of the unit, so an unqualified prelude declaration would
# register under the module's package and be a *different* type from the
# builtin. NCSurface (t/lib-nativecall-surface/) asks from in there.
is-deeply surface-names(), ('bool', 'ssize_t', 'void', 'Pointer'),
    'the type objects are the global ones inside a unit module';
ok opaque-is-pointer(), 'and OpaquePointer is still Pointer there';
is managed-name(), 'NativeCall::CStr', 'explicitly-manage works inside a unit module';
is refreshed(), 1, 'and so does refresh';
