use Test;

plan 8;

# A `sub` declared in a routine or block body is lexical to that body, so a
# same-named routine in a *sibling* body is a fresh declaration, not a
# redeclaration -- even when the two disagree about `multi`.
my @seen;
sub run(&body) { body() }

run {
    sub f($x) { @seen.push("single-$x") }
    f(1);
}
run {
    multi f($x)      { @seen.push("multi-$x") }
    multi f(Str $s)  { @seen.push("multi-str-$s") }
    f(2);
    f('x');
}
is-deeply @seen, ['single-1', 'multi-2', 'multi-str-x'],
    'sibling blocks each get their own routine';

@seen = ();
sub a { sub g($x) { @seen.push("a-$x") }; g(1) }
sub b { multi g($x) { @seen.push("b-$x") }; g(2) }
a();
b();
is-deeply @seen, ['a-1', 'b-2'], 'sibling sub bodies do the same';

# Nested one level deeper, which is the shape roast/S12-subset/subtypes.t uses
# (a subtest inside a subtest).
@seen = ();
run {
    run { sub h($x) { @seen.push("h1-$x") }; h(1) }
    run { multi h($x) { @seen.push("h2-$x") }; h(2) }
}
is-deeply @seen, ['h1-1', 'h2-2'], 'nested sibling blocks too';

# The multi takes the name over inside its own scope rather than letting the
# sibling's single keep answering.
@seen = ();
run { sub k($x) { @seen.push("single") }; k(1) }
run { multi k(Int $) { @seen.push("multi-int") }; k(1) }
is-deeply @seen, ['single', 'multi-int'], 'the multi wins in its own scope';

# A genuine redeclaration inside ONE scope is still an error, at every depth.
throws-like 'sub f1() {say 1}; multi f1() {say 2}', X::Redeclaration,
    'compunit-level redeclaration still throws';
throws-like 'sub o1 { sub f2() {say 1}; multi f2() {say 2} }; o1()',
    X::Redeclaration, 'redeclaration inside one sub body still throws';
throws-like 'sub r1(&c){c()}; r1 { sub f3() {say 1}; multi f3() {say 2} }',
    X::Redeclaration, 'redeclaration inside one block still throws';

# Several `multi`s of one name in one body are one routine, not a conflict.
lives-ok { EVAL 'sub r2(&c){c()}; r2 { multi f4(Int $){}; multi f4(Str $){} }' },
    'sibling multis in one body are fine';
