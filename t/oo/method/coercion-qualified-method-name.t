use v6;
use Test;

# #12934 (Font::FreeType `t/01-face.t`, `my Pointer() $ = $face`): a coercion
# type calls the method named by the target type's full name, so `method
# NativeCall::Types::Pointer { ... }` answers `Pointer()` (whose written name is
# an alias) and `method M::T { ... }` answers `M::T()`. A qualified method name
# is a literal name here, not a qualified call. Every expectation is Rakudo
# v2026.09's output.

use NativeCall;

plan 8;

class F {
    has Pointer $.raw = Pointer.new(42);
    method NativeCall::Types::Pointer { $!raw }
}

{
    my Pointer() $p = F.new;
    isa-ok $p, Pointer, 'a variable coerces through the full-name method';
    is +$p, 42, '... to the pointer it returns';
}

sub takes-pointer(Pointer() $q) { +$q }
is takes-pointer(F.new), 42, 'a parameter coerces too';

module M { class T { has $.v } }
class H { method M::T { M::T.new(v => 3) } }

{
    my M::T() $t = H.new;
    isa-ok $t, M::T, 'a qualified target type';
    is $t.v, 3, '... through its method';
}

is H.new."M::T"().v, 3, 'the method is callable by its literal name';

class A { has $.v }
class B { method A { A.new(v => 7) } }
{
    my A() $x = B.new;
    is $x.v, 7, 'an unqualified target is unchanged';
}

throws-like { my M::T() $t = 42 }, X::Coerce::Impossible,
    'no coercion method is still an impossible coercion';
