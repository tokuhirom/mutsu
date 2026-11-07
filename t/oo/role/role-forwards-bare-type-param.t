use v6;
use Test;

# A parametric role naming one of its OWN type parameters (bare, no `::`) as an
# argument of a role it composes forwards that parameter, exactly as
# `does Base[::R]` does. mutsu used to evaluate the bareword at declaration
# time, get a Str, and fix the parent's parameter to `Str` for every later
# call (found via the Monad distribution, t/13_subtype_hooks).

plan 8;

role Base[::R = Any] {
    method right(R $value) { $value }
    method show { R.^name }
}
role Sub[::R = Any] does Base[R] { }

is Sub.right('e'), 'e', 'first call with a Str';
is Sub.right(9), 9, 'a later call with an Int is not bound to Str';
is Sub.right(Any), Any, 'a type object also binds';
is Sub.show, 'Any', 'the forwarded parameter keeps its default';

role Pair[::A = Any, ::B = Any] {
    method first-of(A $x) { $x }
    method second-of(B $y) { $y }
}
role Wrap[::A = Any, ::B = Any] does Pair[A, B] { }

is Wrap.first-of(1), 1, 'two forwarded parameters, first';
is Wrap.first-of('s'), 's', 'two forwarded parameters, first again';
is Wrap.second-of(2.5), 2.5, 'two forwarded parameters, second';

# A concrete argument still constrains.
role Strict does Base[Str] { }
throws-like { Strict.right(9) }, X::TypeCheck::Binding::Parameter,
    'a concrete Str argument still rejects an Int';

