use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# A builtin that mutsu provides from a prelude (the parametric `Rational` role)
# is switched on by the names an EVAL'd unit uses, including a bare type name
# in term position (`Rational[Int,Int].new`). Checked against rakudo.

plan 6;

my $code = Q[my $q = Rational[Int,Int].new(3, 10); ($q.numerator, $q.denominator)];
is-deeply EVAL($code), (3, 10), 'a parameterised Rational as a term (source)';
is-deeply EVAL($code.AST), (3, 10), 'and as EVAL($ast)';

my $plain = Q[Rational[Int,Int].new(1, 4).numerator];
is EVAL($plain), 1, 'a bare term call (source)';
is EVAL($plain.AST), 1, 'and as EVAL($ast)';

# A class whose name merely starts with `Rational` is not a `role Rational`
# declaration: the prelude is still switched on for `does Rational[UInt]`.
my $named = Q[class RationalThing does Rational[UInt] { }; RationalThing.new(1, 4).Str];
is EVAL($named), '0.25', 'a class named RationalThing does Rational[UInt] (source)';
is EVAL($named.subst('RationalThing', 'RationalOther', :g).AST), '0.25', 'and as EVAL($ast)';
