use Test;
use experimental :rakuast;

plan 10;

# A routine used as an infix (`1 [&plus] 2`) is an `ApplyInfix` over a
# `FunctionInfix`, as rakudo builds it (#18469).

sub expr-of(Str $code) { $code.AST.statements[*-1].expression }

my $plus = Q[sub plus($x, $y) { $x + $y }; 1 [&plus] 2];
my $e = expr-of($plus);
is $e.^name, 'RakuAST::ApplyInfix', 'the application';
is $e.infix.^name, 'RakuAST::FunctionInfix', 'its infix';
is $e.infix.function.^name, 'RakuAST::Var::Lexical', 'the routine term';
is $plus.AST.DEPARSE.lines[*-1], '1 [&plus] 2', 'DEPARSE';
is $plus.AST.EVAL, 3, 'EVAL calls the routine';

my $chain = Q[sub plus($x, $y) { $x + $y }; 1 [&plus] 2 [&plus] 3];
is expr-of($chain).left.infix.^name, 'RakuAST::FunctionInfix', 'a chain nests to the left';
is $chain.AST.EVAL, 6, 'EVAL of the chain';

my $lexical = Q[my &f = -> $a, $b { $a ~ $b }; 1 [&f] 2];
is expr-of($lexical).infix.^name, 'RakuAST::FunctionInfix', 'a lexical &f';
is $lexical.AST.EVAL, '12', 'EVAL of a lexical &f';

sub cat($a, $b) { "$a-$b" }
is (1 [&cat] 2), '1-2', 'the operator still calls the routine';
