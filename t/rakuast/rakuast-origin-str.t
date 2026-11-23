use Test;
use experimental :rakuast;

plan 7;

# `RakuAST::Origin.Str` is the source text of the origin's range, as in
# rakudo (#18312).

my $ast = 'my $y = 1; my $x = try $y.foo;'.AST;
my $decl = $ast.statements[1].expression;
is $ast.statements[1].origin.Str, 'my $x = try $y.foo', 'a statement';
is $decl.origin.Str, 'my $x = try $y.foo', 'a declaration';
is $decl.initializer.expression.origin.Str, 'try $y.foo', 'a prefix';
is "[{$ast.statements[0].origin}]", '[my $y = 1]', 'interpolation';

my @texts;
sub walk($n) { @texts.push: $n.origin.Str with $n.origin; $n.visit-children(&walk) }
walk Q|"x{1 + 2}"|.AST;
ok @texts.grep('1 + 2'), 'a term inside an interpolated block';
@texts = ();
walk $ast;
ok @texts.grep('$y.foo'), 'a postfix';
is Q|my $a = 'é'; my $b = 2|.AST.statements[1].origin.Str, 'my $b = 2',
    'offsets count characters';
