use Test;

# A user variable trait spelled like a parser marker (`is __x`) is a
# `Trait::Is` like any other, and it round-trips through EVAL. Measured on
# rakudo 2026.09.

plan 5;

my $decl = 'multi trait_mod:<is>(Variable:D $v, :$__x!) { }; ';
my $ast = ($decl ~ 'my $a is __x;').AST;
my $var = $ast.statements[1].expression;
isa-ok $var, RakuAST::VarDeclaration::Simple, 'the declaration is a VarDeclaration::Simple';
is $var.traits.elems, 1, 'it has one trait';
isa-ok $var.traits[0], RakuAST::Trait::Is, 'a Trait::Is';
is $var.traits[0].name.canonicalize, '__x', 'named __x';

my @seen;
multi trait_mod:<is>(Variable:D $v, :$__y!) { @seen.push: "y:$__y" }
EVAL(('my $b is __y(5);').AST);
is @seen.join(','), 'y:5', 'the lowered tree applies the handler with its argument';
