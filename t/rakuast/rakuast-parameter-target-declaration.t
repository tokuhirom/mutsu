use Test;
use experimental :rakuast;

# #15780: `ParameterTarget::Var.declaration` answers the VarDeclaration::Simple
# the target declares, at the target's own range. mutsu answered an undefined
# value unless a resolution pass had run.

plan 16;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }
sub target($src, $i = 0) { $src.AST.statements[0].expression.signature.parameters[$i].target }

my $t = target('sub f($a, |c) { 1 }');
isa-ok $t.declaration, RakuAST::VarDeclaration::Simple, 'a scalar target declares a VarDeclaration::Simple';
is $t.declaration.sigil, '$', 'its sigil';
is $t.declaration.desigilname.canonicalize, 'a', 'its name';
is rng($t.declaration), '6-8', 'at the target\'s range';
is rng($t.declaration), rng($t), 'which equals the target\'s';

is target('sub f(@a) { 1 }').declaration.sigil, '@', 'an array target';
is target('sub f(%h) { 1 }').declaration.sigil, '%', 'a hash target';
is target('sub f(:$n) { 1 }').declaration.desigilname.canonicalize, 'n', 'a named parameter';
is target('sub f($*dyn) { 1 }').declaration.twigil, '*', 'a twigil is kept';
is target('sub f($*dyn) { 1 }').declaration.desigilname.canonicalize, 'dyn', 'and is not part of the name';

is target('sub f($a, $b) { 1 }', 1).declaration.desigilname.canonicalize, 'b', 'the second parameter';
is rng(target('sub f($a, $b) { 1 }', 1).declaration), '10-12', 'and its range';
# (mutsu also prints the implicit `type`, which rakudo keeps but does not print)
ok target('sub f($a = 3) { 1 }').declaration.raku.contains('desigilname => RakuAST::Name.from-identifier("a")'),
    'the declaration prints its name';

my $pointy = 'my $x = -> $a, $b { 1 }'.AST.statements[0].expression.initializer.expression.signature.parameters[1].target;
is rng($pointy.declaration), '15-17', 'a pointy block parameter';
is $pointy.declaration.desigilname.canonicalize, 'b', 'and its name';

# a slurpy capture's target is a ParameterTarget::Term, which has no `declaration`
is target('sub f($a, |c) { 1 }', 1).^name, 'RakuAST::ParameterTarget::Term', 'a capture target is a Term target';
