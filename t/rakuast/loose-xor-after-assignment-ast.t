use Test;
use experimental :rakuast;

plan 10;

# The loose `xor` after an assignment or a declaration keeps its spelling
# (`Infix("xor")`, as rakudo), while a tight `^^` stays `^^` (#18340).

for 'my $y = 1 xor 2', 'my $y = 1 xor 2 xor 3', 'my @a = 1, 2 xor 3',
    'my $y = 1 ^^ 2', 'my $y = 1 ^^ 2 xor 3', 'my $y = 1 xor 2 and 3' -> $code {
    is $code.AST.DEPARSE.trim, $code, "DEPARSE of $code";
}

is Q[my $x; $x = 1 xor 2].AST.statements[1].expression.infix.operator, 'xor',
    'an assignment statement';
is Q[my $x; ($x = 1) ^^ 2].AST.statements[1].expression.infix.operator, '^^',
    'a parenthesized assignment keeps a tight ^^';

is Q[my $y = 1 xor 0; $y].AST.EVAL, 1, 'EVAL: the declaration takes 1';
is Q[my $x; $x = 0 xor 3; $x].AST.EVAL, 0, 'EVAL: the assignment takes 0';
