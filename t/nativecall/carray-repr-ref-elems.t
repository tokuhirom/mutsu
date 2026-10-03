use Test;
use nqp;
use NativeCall;

# A reference-element `is repr('CArray')` class (#11209, ADR-0015 P3c): a C
# array of addresses (`char**`, `void**`) whose slots read back as objects.
# This mirrors upstream `NativeCall::Types`' `TypedCArray` role, whose element
# access is `nqp::atpos`/`nqp::bindpos` on `self`.

plan 21;

class Ptr is repr('CPointer') {
    method Int { nqp::unbox_i(self) }
}
class CA is repr('CArray') is array_type(Ptr) {
    my role Typed[::TValue] is array_type(TValue) {
        method AT-POS(::?CLASS:D: Int:D $pos) { nqp::atpos(self, nqp::unbox_i($pos)) }
        method ASSIGN-POS(::?CLASS:D: Int:D $pos, \value) {
            nqp::bindpos(self, nqp::unbox_i($pos), nqp::decont(value))
        }
    }
    method ^parameterize(Mu:U \array, Mu:U \t) {
        my $what := array.^mixin(Typed[t.WHAT]);
        $what.^set_name(array.^name ~ '[' ~ t.^name ~ ']');
        $what
    }
    method elems(::?CLASS:D:) { nqp::elems(self) }
}

# Str elements.
my $s := nqp::create(CA[Str]);
is nqp::elems($s), 0, 'a fresh Str array is empty';
nqp::bindpos($s, 2, 'two');
is nqp::elems($s), 3, 'binding past the end grows it';
is nqp::atpos($s, 2), 'two', 'a bound Str reads back';
ok nqp::atpos($s, 0) =:= Str, 'a NULL slot reads as the element type object';
ok nqp::atpos($s, 9) =:= Str, 'so does a slot past the end';
nqp::bindpos($s, 2, Str);
ok nqp::atpos($s, 2) =:= Str, 'binding the type object stores NULL';

# Indexing a mixin instance dispatches the role's ASSIGN-POS/AT-POS instead
# of replacing the variable with an Array.
my $v = CA[Str].new;
$v[0] = 'zero';
$v[1] = 'one';
is $v.^name, 'CA[Str]', 'element assignment keeps the object';
is $v[1], 'one', 'and the element reads back through AT-POS';
is $v.elems, 2, 'elems counts the slots';

# The slots are real `char*`s, owned by the array: read as C memory, they
# are the strings.
my $as-c = nativecast(CArray[Str], nativecast(Pointer, $v));
is $as-c[0], 'zero', 'slot 0 holds the address of a C string';
is $as-c[1], 'one', 'slot 1 too';

# Pointer elements.
my $p := nqp::create(CA[Ptr]);
my $ptr = nqp::box_i(42, Ptr);
isa-ok $ptr, Ptr, 'nqp::box_i boxes into a CPointer class';
nqp::bindpos($p, 1, $ptr);
ok nqp::atpos($p, 1) === $ptr, 'a bound object reads back as itself';
is nqp::atpos($p, 1).Int, 42, 'with its address';
ok nqp::atpos($p, 0) =:= Ptr, 'an unbound slot is the type object';

# A slot C rewrites reads back as a new object at the new address.
sub strtol(Pointer, Pointer, int32 --> long) is native {*}
my $text = CA[Str].new;
$text[0] = '12abc';
my $start = nativecast(CArray[Pointer], nativecast(Pointer, $text))[0];
my $end := nqp::create(CA[Ptr]);
nqp::bindpos($end, 0, $ptr);
is strtol($start, nativecast(Pointer, $end), 10), 12, 'C parsed the owned string';
my $after = nqp::atpos($end, 0);
ok $after !=== $ptr, 'the stale object is not answered';
isa-ok $after, Ptr, 'the slot is rematerialised as the element type';
is $after.Int - $start.Int, 2, 'at the address C wrote';

# A REPR of a type constraint that parameterizes through `^parameterize`.
sub takes(CA[Str] $a) { $a.elems }
is takes($v), 2, 'a CA[Str] argument binds to a CA[Str] parameter';
sub takes-ptr(CA[Ptr] $a) { 'bound' }
dies-ok { takes-ptr($v) }, 'but not to a CA[Ptr] parameter';
