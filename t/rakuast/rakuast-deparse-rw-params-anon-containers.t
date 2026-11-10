use Test;
plan 14;
sub dp(Str $src) { $src.AST.DEPARSE.chomp.subst(/\s+/, ' ', :g) }

# `is rw` on a parameter is a trait, not `<->`.
is dp('my @a; for @a -> $x is rw, $y { }'), 'my @a; for @a -> $x is rw, $y { }', 'is rw on the first of two parameters';
is dp('my @a; for @a -> $x, $y is rw { }'), 'my @a; for @a -> $x, $y is rw { }', 'is rw on the second';
is dp('my @a; for @a <-> $x, $y { }'), 'my @a; for @a <-> $x, $y { }', '<-> keeps default-rw';

# A bare declared `@` / `%` is an anonymous declaration.
is dp('say (my int @).of'), 'say (my int @).of', 'a typed anonymous array';
is dp('say (my %).of'), 'say (my %).of', 'an anonymous hash';
is dp('my @ = 1, 2; say 3'), 'my @ = 1, 2; say 3', 'an anonymous array with an initializer';
is EVAL(Q[(my int @).of].AST), int, 'EVAL of an anonymous native array';
is EVAL(Q[(my uint8 @ = 1, 2).of].AST), uint8, 'EVAL of an initialized anonymous native array';
is EVAL(Q[my %; 5].AST), 5, 'EVAL of an anonymous hash declaration';
is Q[my @].AST.statements[0].expression.^name, 'RakuAST::VarDeclaration::Anonymous', 'Anonymous node';

# `:$!attr` binds under the plain name.
is dp('class C { has $.s; has @.r; submethod TWEAK (:$!s, :@!r) { } }'), 'class C { has $.s; has @.r; submethod TWEAK (:s($!s), :r(@!r)) { } }', 'named attribute parameters';
my $ast = Q[class Dq { has $.x; submethod BUILD(:$!x) { } }; Dq.new(x => 7).x].AST;
is EVAL($ast), 7, 'EVAL binds the attribute through the named parameter';
my $p = Q[class Eq { has $.x; submethod BUILD(:$!x) { } }].AST.statements[0].expression.body.body.statement-list.statements[1].expression.signature.parameters[0];
is $p.names.join(','), 'x', 'the parameter name list';
is $p.target.name, '$!x', 'the parameter target';
