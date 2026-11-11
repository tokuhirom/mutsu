use Test;

# A role whose type parameters all have defaults is punned with those
# defaults; the parametrization is remembered per registry state rather than
# re-bound on every `R.new` / `R.method` (Monad's `Monad::Maybe.some`, #17331).
# Repeated calls must keep answering exactly what the first one did, and a
# role declared later must get its own parametrization.

plan 10;

role Box[::T = Int] {
    has $.value;
    method type-arg { T }
    method make($v) { self.new(value => $v) }
}

my @types = (^50).map({ Box.new(value => $_).type-arg });
is @types.unique.elems, 1, 'every pun binds the same default';
is @types[0].^name, 'Int', 'and it is the declared default';

my $sum = 0;
$sum += Box.make($_).value for ^50;
is $sum, 1225, 'a method called on the role puns it each time';

ok Box.new(value => 1).WHAT === Box.new(value => 2).WHAT, 'the pun is one type';
ok Box.new(value => 1) ~~ Box, 'an instance does the role';

role Pair2[::K = Str, ::V = Int] {
    method kinds { K.^name ~ '/' ~ V.^name }
}
is Pair2.new.kinds, 'Str/Int', 'two defaults';
is Pair2.new.kinds, 'Str/Int', 'two defaults, again';

role Later[::T = Str] { method t { T.^name } }
is Later.new.t, 'Str', 'a role declared after the first puns its own default';
is Box.new(value => 0).type-arg.^name, 'Int', 'the earlier role keeps its own';

is Box[Str].new(value => 'x').type-arg.^name, 'Str', 'an explicit argument still wins';
