use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# A builtin that mutsu provides from a prelude (the parametric `Rational` role)
# is switched on by the names an EVAL'd unit uses, including a bare type name
# in term position (`Rational[Int,Int].new`). Checked against rakudo.

plan 4;

my $code = Q[my $q = Rational[Int,Int].new(3, 10); ($q.numerator, $q.denominator)];
is-deeply EVAL($code), (3, 10), 'a parameterised Rational as a term (source)';
is-deeply EVAL($code.AST), (3, 10), 'and as EVAL($ast)';

my $plain = Q[Rational[Int,Int].new(1, 4).numerator];
is EVAL($plain), 1, 'a bare term call (source)';
is EVAL($plain.AST), 1, 'and as EVAL($ast)';
