# An untyped routine parameter is implicitly `Any`, so it rejects the `Mu`
# type object ("expected Any but got Mu"), while a block's untyped parameter is
# implicitly `Mu` and accepts it (#10878). Every call path has to agree: the
# general binder, the positional-light and typed-light sub paths, the TRIR
# entry (a hot sub), the method fast path, and named parameters.
use Test;

plan 16;

sub msg(&code) {
    code();
    return 'no error';
    CATCH { default { return .message } }
}

my $want = "Type check failed in binding to parameter '\$x'; expected Any but got Mu (Mu)";

sub untyped($x) { $x }
is msg({ untyped(Mu) }), $want, 'untyped positional rejects Mu';
is msg({ untyped(Mu) for ^3 }), $want, 'repeated (hot) calls reject Mu too';

sub two($y, $x) { $x }
is msg({ two(1, Mu) }), $want, 'second untyped positional rejects Mu';

sub typed-then-untyped(Int $y, $x) { $x }
is msg({ typed-then-untyped(1, Mu) }), $want, 'typed-light path rejects Mu';

sub explicit-any(Any $x) { $x }
is msg({ explicit-any(Mu) }), $want, 'explicit Any rejects Mu at run time';

sub sigilless(\x) { x }
like msg({ sigilless(Mu) }), /'expected Any but got Mu'/, 'sigilless parameter rejects Mu';

sub named(:$x) { $x }
is msg({ named(:x(Mu)) }), $want, 'untyped named rejects Mu';

class C {
    method m($x) { $x }
    method n(:$x) { $x }
}
is msg({ C.m(Mu) }), $want, 'method on a type object rejects Mu';
is msg({ C.new.m(Mu) }), $want, 'method on an instance rejects Mu';
is msg({ C.new.n(:x(Mu)) }), $want, 'method named parameter rejects Mu';

# What stays accepted.
sub mu-typed(Mu $x) { $x }
is mu-typed(Mu).raku, 'Mu', 'an explicit Mu parameter accepts Mu';
my &b = -> $x { $x };
is b(Mu).raku, 'Mu', 'a block parameter is implicitly Mu';
is untyped(Any).raku, 'Any', 'Any itself still binds';
is untyped(Int).raku, 'Int', 'a type object below Any still binds';
is untyped(1|2).raku, 'any(1, 2)', 'a Junction still autothreads';

# `Mu` is a supertype of every parameter type, so the failure is the binder's
# run-time error, not a compile-time "will never work".
sub int-param(Int $x) { $x }
like msg({ int-param(Mu) }), /^'Type check failed in binding'/,
    'a Mu argument is not refuted at compile time';
