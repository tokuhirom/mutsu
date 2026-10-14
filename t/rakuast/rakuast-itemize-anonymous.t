use v6;
use experimental :rakuast;
use Test;

# `$@` / `$%` are an item contextualizer over an anonymous variable; the AST
# must EVAL back (measured against rakudo 2026.09).

plan 5;

my $ast = Q|$@|.AST;
my $expr = $ast.statements[0].expression;
is $expr.^name, 'RakuAST::Contextualizer::Item', '$@ is Contextualizer::Item';
is $expr.target.^name, 'RakuAST::VarDeclaration::Anonymous', 'over an anonymous declaration';
is $expr.target.sigil, '@', 'with the @ sigil';
is $expr.target.scope, 'state', 'in state scope';
lives-ok { EVAL $ast }, 'EVAL of the AST lives';
