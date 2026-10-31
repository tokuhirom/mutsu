use v6;
use Test;

# `is DEPRECATED(EXPR)`: the message is the value of EXPR, evaluated when the
# routine is declared, whatever EXPR is (a lexical, a concatenation, a call).
# Such a unit must survive the RakuAST round trip (#15632). Expected output is
# rakudo 2026.09's.

plan 7;

my constant $msg = "use X instead";
sub plain-text(--> Str) { "from a call" }

class K {
    multi method lp(Int $n --> Int) is DEPRECATED($msg) { 1 }
    multi method lp(Str $s --> Int) is DEPRECATED("m" ~ "2") { 2 }
    method call(--> Int) is DEPRECATED(plain-text()) { 3 }
}
sub old(--> Int) is DEPRECATED($msg) { 4 }
sub older(--> Int) is DEPRECATED("lit") { 5 }

is K.new.lp(1), 1, 'a method with a constant message runs';
is K.new.lp('a'), 2, 'a method with a concatenated message runs';
is K.new.call, 3, 'a method with a call message runs';
is old(), 4, 'a sub with a constant message runs';
is older(), 5, 'a sub with a literal message runs';

my $report = Deprecation.report;
like $report, /'Please use use X instead instead.'/, 'the constant is the message';
like $report, /'Please use from a call instead.'/, 'a call is evaluated for the message';
