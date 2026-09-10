use Test;

# A role's type capture can carry a definiteness smiley in a parameter type:
# `role R[::T] { method f(T:D $x) }`. Role-method registration validated the
# whole constraint against the role's type-parameter list, so only a bare `T`
# matched and `T:D` was rejected as "Invalid typename 'T:D' in parameter
# declaration." — which stopped `NativeHelpers::CStruct`'s `LinearArray[::T]`
# from loading at all.

plan 8;

role Holder[::T] {
    method take(T:D $x) { "def:$x" }
    method kind(T:U $t) { 'type:' ~ $t.^name }
    method any(T $x) { "any:{$x // 'undef'}" }
    multi method pick(T:D $x) { "multi-def:$x" }
    multi method pick(T:U) { 'multi-type' }
}

class IntHolder does Holder[Int] { }
my $h = IntHolder.new;

is $h.take(5), 'def:5', 'a `T:D` parameter accepts a defined value';
is $h.kind(Int), 'type:Int', 'a `T:U` parameter accepts the type object';
is $h.any(7), 'any:7', 'a bare `T` parameter still works';
is $h.any(Int), 'any:undef', 'a bare `T` parameter accepts a type object';
is $h.pick(9), 'multi-def:9', 'a `T:D` multi candidate';
is $h.pick(Int), 'multi-type', 'a `T:U` multi candidate';

# The smiley must still constrain: a type object cannot bind to `T:D`.
dies-ok { $h.take(Int) }, 'a type object does not bind to `T:D`';

# A second instantiation gets its own constraint.
class StrHolder does Holder[Str] { }
is StrHolder.new.take('x'), 'def:x', 'the constraint follows the instantiation';
