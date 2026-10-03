use v6;
use Test;

# A single `&f` pointy-block parameter is a real `&f` lexical: it shadows a
# same-named sub for `&f()`, a bare `f()` and `&f` as a value. The one-parameter
# block used to drop the `&` sigil, so the body never saw the binding (#10997).

plan 5;

sub f() { 'sub' }

is (-> &f { &f() })(-> { 'arg' }), 'arg', '&f() calls the argument';
is (-> &f { f() })(-> { 'arg' }), 'arg', 'f() calls the argument';
is (-> &f { &f.name })(sub named { }), 'named', '&f as a value is the argument';
is (-> &f { f(3) })(* + 1), 4, 'arguments are passed through';
is (-> &f { &f.signature.arity })(-> $a, $b { }), 2, 'the Callable is bound unchanged';
