use Test;

# Imaginary literals in RakuAST, measured on rakudo 2026.09: the number under
# a `Postfix("i")`, `ApplyPostfix(IntLiteral(2), Postfix("i"))`. EVAL of the
# tree computes the same Complex as the parsed literal.

plan 10;

sub expr($src) { $src.AST.statements.head.expression }

my $two = expr(Q[2i]);
isa-ok $two, RakuAST::ApplyPostfix, '`2i` applies a postfix';
isa-ok $two.operand, RakuAST::IntLiteral, 'to the integer';
is $two.postfix.operator, 'i', 'the `i` postfix';
isa-ok expr(Q[3.5i]).operand, RakuAST::RatLiteral, '`3.5i` holds a Rat';
isa-ok expr(Q[1+2i]).right, RakuAST::ApplyPostfix, '`1+2i` adds the imaginary literal';

sub run($src) { EVAL($src.AST) }
is run(Q[2i]), Complex.new(0, 2), '`2i`';
is run(Q[1+2i]), Complex.new(1, 2), '`1+2i`';
is run(Q[3.5i * 2]), Complex.new(0, 7), '`3.5i` in an expression';
isa-ok run(Q[0i]), Complex, '`0i` is still a Complex';
is run(Q[0.25i]).im, 0.25, 'a fractional imaginary part';
