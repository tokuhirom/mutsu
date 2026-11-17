use experimental :rakuast;
use Test;

# An imaginary literal keeps the number it was written with, as the literal
# operand of a `Postfix("i")`, as rakudo's RakuAST frontend has it: `0.0i` is a
# `RatLiteral`, `3e2i` a `NumLiteral`, `2i` an `IntLiteral` (#14880, the
# `.DEPARSE` corpus's S32-trig files). The value is unchanged.

plan 14;

sub operand($src) { $src.AST.statements[0].expression.operand }
sub deparse($src) { $src.AST.statements[0].expression.DEPARSE }

isa-ok operand('0.0i'), RakuAST::RatLiteral, '0.0i: a Rat';
is deparse('0.0i'), '0.0i', 'and deparses as written';
isa-ok operand('2i'), RakuAST::IntLiteral, '2i: an Int';
isa-ok operand('1.5e0i'), RakuAST::NumLiteral, '1.5e0i: a Num';
is deparse('3e2i'), '300e0i', 'a Num deparses as its value';
is deparse('2.50i'), '2.5i', 'and a Rat';
is deparse('1_000i'), '1000i', 'underscores are not kept';
is deparse('.5i'), '0.5i', 'a leading dot';

is 0.0i, Complex.new(0, 0), 'the value of 0.0i';
is 2.0i + 1, Complex.new(1, 2), '2.0i in an expression';
is -0.5i, Complex.new(0, -0.5), 'a negated one';
isa-ok 3e2i, Complex, 'still a Complex';
is EVAL('2.50i * 2'.AST), Complex.new(0, 5), 'the view runs through EVAL';
is EVAL('1.5e0i'.AST), Complex.new(0, 1.5), 'with a Num operand too';
