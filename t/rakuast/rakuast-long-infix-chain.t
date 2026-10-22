use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 3;

# A long left-nested infix chain (roast/S32-num/int.t has 300+ `~` operands)
# is an ordinary program: the walks over the tree are bounded by the real
# stack headroom, not by a small fixed nesting counter.
my $n = 400;
my $source = "my \$s = 'a'" ~ (" ~ 'b'" x $n) ~ "; \$s.chars";
my $ast = $source.AST;
isa-ok $ast, RakuAST::StatementList, 'a 400-operand chain converts to RakuAST';
is EVAL($ast), $n + 1, 'the converted chain evaluates';
is EVAL($source), $n + 1, 'the same source evaluates directly';
