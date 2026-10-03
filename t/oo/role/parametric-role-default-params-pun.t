use Test;

# A method called on a parametric role's type object puns the role to its
# default parameterization when every type parameter has a default, the same
# way `.new` does. Found via the Monad distribution, whose
# `role Monad::Either[::L = Any, ::R = Any]` is used as
# `Monad::Either.right(21)`.

plan 7;

role Either[::L = Any, ::R = Any] {
    has $.value;
    method right(R $value) { self.new(:$value) }
    method left-type { L.^name }
    method right-type { R.^name }
}

is Either.right(21).value, 21, 'a type-capture parameter with a default binds on a pun';
is Either.left-type, 'Any', 'the first defaulted type capture is bound';
is Either.right-type, 'Any', 'the second defaulted type capture is bound';
is Either[Str, Int].right-type, 'Int', 'an explicit parameterization still wins';

role Typed[::T = Int] { method take(T $v) { $v } }
throws-like { Typed.take('s') }, X::TypeCheck::Binding,
    'the default type constrains the punned method';

role Valued[$x = 5] { method get { $x } }
is Valued.get, 5, 'a value parameter takes its default on a pun';
is Valued[7].get, 7, 'an explicit value argument still wins';
