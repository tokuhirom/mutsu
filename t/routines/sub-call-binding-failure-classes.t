use Test;

# A plain-sub call whose argument fails a parameter's type raises rakudo's
# run-time X::TypeCheck::Binding::Parameter, naming the object's own class and
# `.raku` -- unless every argument's type is known at compile time (a literal,
# a type object, a variable declared with a type, no named argument), which is
# the one shape rakudo rejects as "Calling f(...) will never work with declared
# signature ..." (X::TypeCheck::Argument). mutsu#10640.

plan 22;

class F {}
class G { has $.x = 1 }

sub m(Int $i) { }
sub a(@a) { }
sub g(&c) { }
sub o(Callable $p) { }
sub h(%h) { }

sub failure(&code) {
    code();
    CATCH { default { return $_ } }
    Nil
}

my $e = failure { m(G.new) };
isa-ok $e, X::TypeCheck::Binding::Parameter, 'object argument: run-time exception';
is $e.message,
    q{Type check failed in binding to parameter '$i'; expected Int but got G (G.new(x => 1))},
    'object argument: names its class and .raku';

$e = failure { a(F.new) };
isa-ok $e, X::TypeCheck::Binding::Parameter, '@ parameter: run-time exception';
is $e.message,
    q{Type check failed in binding to parameter '@a'; expected Positional but got F (F.new)},
    '@ parameter: names the object';

$e = failure { g(F.new) };
isa-ok $e, X::TypeCheck::Binding::Parameter, 'untyped & parameter rejects a non-Callable';
is $e.message,
    q{Type check failed in binding to parameter '&c'; expected Callable but got F (F.new)},
    '& parameter: names the object';

$e = failure { o(F.new) };
isa-ok $e, X::TypeCheck::Binding::Parameter, 'Callable $p: run-time exception';
is $e.message,
    q{Type check failed in binding to parameter '$p'; expected Callable but got F (F.new)},
    'Callable $p: names the object';

$e = failure { h(F.new) };
isa-ok $e, X::TypeCheck::Binding::Parameter, '% parameter: run-time exception';
is $e.message,
    q{Type check failed in binding to parameter '%h'; expected Associative but got F},
    '% parameter: names the type only, as rakudo does';

my $untyped = "x";
$e = failure { m($untyped) };
isa-ok $e, X::TypeCheck::Binding::Parameter, 'untyped variable: run-time exception';
is $e.message,
    q{Type check failed in binding to parameter '$i'; expected Int but got Str ("x")},
    'untyped variable: run-time message';

$e = failure { m(1 + 1.5) };
isa-ok $e, X::TypeCheck::Binding::Parameter, 'computed argument: run-time exception';

$e = failure { m($untyped, 2) };
ok $e ~~ X::AdHoc && $e.message eq 'Too many positionals passed; expected 1 argument but got 2',
    'arity error with a run-time argument: the plain run-time message';

isa-ok failure({ m(Str:D) }), X::TypeCheck::Binding::Parameter,
    'a definite-type object is checked at run time';

g(-> {});
pass 'untyped & parameter still binds a Callable';

# Statically typed call shapes: rakudo's compile-time X::TypeCheck::Argument.
throws-like 'sub m(Int $i) { }; m("x")', X::TypeCheck::Argument,
    message => 'Calling m(Str) will never work with declared signature (Int $i)',
    'string literal';
throws-like 'class K {}; sub m(Int $i) { }; m(K)', X::TypeCheck::Argument,
    message => 'Calling m(K) will never work with declared signature (Int $i)',
    'type object names its class';
throws-like 'sub m(Int $i) { }; my Str $s = "a"; m($s)', X::TypeCheck::Argument,
    message => 'Calling m(Str) will never work with declared signature (Int $i)',
    'variable declared with a type';
throws-like 'sub m(Int $i) { }; m()', X::TypeCheck::Argument,
    message => 'Calling m() will never work with declared signature (Int $i)',
    'no arguments';
throws-like 'sub m(Int $i) { }; m(1, 2)', X::TypeCheck::Argument,
    message => 'Calling m(Int, Int) will never work with declared signature (Int $i)',
    'too many literal arguments';
throws-like 'sub m(Int $i) { }; m(Nil)', X::TypeCheck::Argument, 'Nil';
