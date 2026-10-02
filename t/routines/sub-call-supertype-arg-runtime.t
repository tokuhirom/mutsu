use Test;

# A statically-typed call is reported as the compile-time "Calling f(...) will
# never work" (X::TypeCheck::Argument) only when no argument could bind. A
# type-object argument that is a supertype of its parameter's type (`Cool` for
# `Int $x`, `Mu` for anything) might, so rakudo leaves the call to the run-time
# binder -- for a type mismatch and an arity mismatch alike (#10944).

plan 12;

sub one(Int $x) { 1 }
sub two(Int $x, Str $y) { 1 }
sub pair-of(Int $x, $y) { 1 }
class A {}
class B is A {}
sub b(B $x) { 1 }

sub failure(&code) {
    code();
    CATCH { default { return $_ } }
    Nil
}

my $e = failure { one(Cool) };
isa-ok $e, X::TypeCheck::Binding::Parameter, 'a supertype type object is a run-time failure';
is $e.message, q{Type check failed in binding to parameter '$x'; expected Int but got Cool (Cool)},
    'with the binder message';

isa-ok failure({ one(Numeric) }), X::TypeCheck::Binding::Parameter, 'a supertype role too';
isa-ok failure({ one(Mu) }), X::TypeCheck::Binding::Parameter, 'Mu is a supertype of every type';
isa-ok failure({ b(A) }), X::TypeCheck::Binding::Parameter, 'a user superclass';
isa-ok failure({ two(Cool, 1) }), X::TypeCheck::Binding::Parameter,
    'one undecidable argument keeps a refutable sibling at run time';

$e = failure { one(Cool, 2) };
nok $e ~~ X::TypeCheck::Argument, 'an arity mismatch with a supertype argument is not refuted';
like $e.message, /'Too many positionals passed'/, 'it is the binder arity error';
like failure({ pair-of(Cool) }).message, /'Too few positionals passed'/, 'too few likewise';

isa-ok (try EVAL 'one(Str)') // $!, X::TypeCheck::Argument, 'an unrelated type object is still refuted';
isa-ok (try EVAL 'two(Int, 1)') // $!, X::TypeCheck::Argument, 'as is an exact type with a bad sibling';
isa-ok (try EVAL 'one(Int, 2)') // $!, X::TypeCheck::Argument, 'and an arity mismatch of exact types';
