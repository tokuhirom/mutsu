use v6;
use experimental :rakuast;
use Test;

# `my $b where { ... } := 7` keeps its `where` block next to the Bind
# initializer, and the AST evals back to the same checked binding.

plan 6;

my $ast = Q{my $b where { $_ > 0 } := 7;}.AST;
my $decl = $ast.statements[0].expression;
is $decl.^name, 'RakuAST::VarDeclaration::Simple', 'a Simple declaration';
is $decl.initializer.^name, 'RakuAST::Initializer::Bind', 'with a Bind initializer';
is $decl.where.^name, 'RakuAST::Block', 'and a where block';

is EVAL(Q{my $b where { $_ > 0 } := 7; $b}.AST), 7, 'a satisfied where binds';
is EVAL(Q{my Int $b where { $_ > 0 } := 8; $b}.AST), 8, 'a typed where binds';
dies-ok { EVAL(Q{my $b where { $_ > 0 } := -1; $b}.AST) }, 'a violated where dies';
