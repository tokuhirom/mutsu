use Test;

# Placeholder variables in RakuAST, measured on rakudo 2026.09. Every kind is a
# `VarDeclaration::Placeholder` node written with the variable's sigil and bare
# name; `@^a`, `%^h` and `&^cb` are all `Positional`, `$:foo` is `Named`. A sub
# declared without a signature takes them as its implicit signature.

plan 24;

sub expr($src) { $src.AST.statements.head.expression }
sub body-expr($src) { expr($src).body.statement-list.statements.head.expression }

# --- read direction ---------------------------------------------------------
my $array = body-expr(Q[{ @^a.elems }]).operand;
isa-ok $array, RakuAST::VarDeclaration::Placeholder::Positional, '@^a is a positional placeholder';
is $array.lexical-name, '@a', 'it keeps the sigil and the bare name';

my $hash = body-expr(Q[{ %^h<z> }]).operand;
isa-ok $hash, RakuAST::VarDeclaration::Placeholder::Positional, '%^h is a positional placeholder';
is $hash.lexical-name, '%h', 'a hash placeholder';

my $code = body-expr(Q[{ &^cb() }]).operand;
isa-ok $code, RakuAST::VarDeclaration::Placeholder::Positional, '&^cb is a positional placeholder';
is $code.lexical-name, '&cb', 'a code placeholder';

my $named = body-expr(Q[{ $:foo }]);
isa-ok $named, RakuAST::VarDeclaration::Placeholder::Named, '$:foo is a named placeholder';
is $named.lexical-name, '$foo', 'it names the variable without the twigil';
is body-expr(Q[{ $:foo + $:foo }]).right.lexical-name, '$foo', 'a second use is a placeholder too';

# A statement-initial `$:foo` is not a bare `$`: no anonymous state variable.
is expr(Q[{ $:foo }]).body.statement-list.statements.elems, 1, 'no state variable is declared';

my $sub = expr(Q[sub a { $:foo }]);
is $sub.body.statement-list.statements.head.expression.lexical-name, '$foo',
    'a named placeholder in a sub body';

# --- write direction --------------------------------------------------------
sub run($src) { EVAL($src.AST) }
is run(Q[{ @^a.elems }.([1, 2, 3])]), 3, 'an array placeholder binds';
is run(Q[{ %^h<z> }.({ z => 9 })]), 9, 'a hash placeholder binds';
is run(Q[{ &^cb() }.({ 7 })]), 7, 'a code placeholder binds';
is run(Q[{ $:foo }.(:foo(4))]), 4, 'a named placeholder binds';
is run(Q[{ "x $:foo" }.(:foo(5))]), 'x 5', 'a named placeholder interpolates';
is run(Q[{ @^a.elems + $^b + $:c }.([1, 2], 10, :c(100))]), 112, 'the three kinds mix';

# A sub without a signature takes the placeholders of its body.
is run(Q[sub a { $:foo }; a(:foo(3))]), 3, 'a sub takes a named placeholder';
is run(Q[sub b { @^a.elems + $^b }; b([1, 2], 5)]), 7, 'a sub takes positional placeholders';
is run(Q[sub c { $^b ~ $^a }; c("x", "y")]), 'yx', 'placeholders are ordered by name';
is run(Q[sub d { "a:$:foo b:$^p" }; d(7, :foo(3))]), 'a:3 b:7', 'both kinds in a string';
throws-like { run(Q[sub e { $:foo }; e(:bar(1))]) }, X::AdHoc,
    'an unexpected named argument is still rejected';

# A sub with an explicit signature keeps it.
is run(Q[sub f($x) { $x * 2 }; f(4)]), 8, 'an explicit signature is untouched';
is run(Q[sub g() { 11 }; g()]), 11, 'an empty signature is untouched';
