use Test;
use experimental :rakuast;

plan 11;

# A superscript power is `Postfix::Power`, not a `**` (#14845).
sub dep($src) { $src.AST.DEPARSE.lines.tail.trim }

is dep('my $x = 2²;'), 'my $x = 2²', 'a literal operand';
is dep('my $y = 3⁴;'), 'my $y = 3⁴', 'a multi-digit-free power';
is dep('my $w = 2⁻¹;'), 'my $w = 2⁻¹', 'a negative power';
is dep('my $v = (1 + 2)³;'), 'my $v = (1 + 2)³', 'a parenthesized operand';
is dep('my $n = 3; my $z = $n⁴ + 1;'), 'my $z = $n⁴ + 1', 'inside a larger expression';
is dep('my $e = 2 ** 2;'), 'my $e = 2 ** 2', 'a written ** is unchanged';

my $p = 'my $x = 2²;'.AST.find-nodes(RakuAST::Postfix::Power)[0];
is $p.power, 2, 'the power';
is $p.raku, 'RakuAST::Postfix::Power.new(2)', '.raku';
is RakuAST::Postfix::Power.new(3).power, 3, 'constructible';

is EVAL('2²'.AST), 4, 'EVAL of the AST';
is EVAL('my $n = 5; $n³ + 2⁻¹'.AST), 125.5, 'EVAL with a negative power';
