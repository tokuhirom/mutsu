use Test;

# Inside a role body the role's own name denotes the role GROUP, also when the
# method runs on the role's pun (`R.m`): `R.^parameterize(T)` builds `R[T]`
# and does not hand the punned class back (Monad::Maybe.some).
plan 9;

role Mn::Mb[::T = Any] {
    method mt(--> Mu) { Mn::Mb }
    method p { Mn::Mb.^parameterize(Any).raku }
}

my $mt = Mn::Mb.mt;
is $mt.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW', 'the name in a pun method is the group';
is $mt.^name, 'Mn::Mb', 'its name';
is $mt.^parameterize(Any).raku, 'Mn::Mb[Any]', '.^parameterize on it builds the curried role';
is Mn::Mb.^parameterize(Any).raku, 'Mn::Mb[Any]', 'same from outside';
is Mn::Mb.p, 'Mn::Mb[Any]', 'parameterize inside the body';
is Mn::Mb[Int].mt.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW', 'on a curried role too';

# `Nil` given to a `has T $.value` of a parametric role resets it to the
# type's default, like a plain typed attribute.
role Nv[::T = Any] {
    has T $.value;
    method g { $!value.gist }
}
is Nv.new(value => Nil).g, '(Any)', 'Nil resets the defaulted T attribute';
is Nv[Int].new(value => Nil).g, '(Int)', 'and the curried one';
class Kv does Nv[Int] {}
is Kv.new(value => Nil).g, '(Int)', 'and a class composing it';
