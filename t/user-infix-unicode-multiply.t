use Test;

# Declaring a `multi infix:<×>` / `infix:<÷>` candidate must NOT disable the
# built-in parse of the Unicode multiply/divide operators. Previously the
# parser skipped `×`/`÷` entirely once a user candidate existed, leaving
# `2 × 3` stranded as "Two terms in a row". They now behave exactly like the
# ASCII `*` / `/`: parsed at multiplicative precedence, with the VM dispatching
# to the user candidate for operands the built-in does not handle.

plan 6;

class Angle { has $.v; method v { $!v } }
multi infix:<×>(Angle $a, Real $b) { Angle.new(v => $a.v × $b) }
multi infix:<×>(Real $a, Angle $b) { Angle.new(v => $a × $b.v) }
multi infix:<÷>(Angle $a, Real $b) { Angle.new(v => $a.v ÷ $b) }

# Built-in numeric × / ÷ still parse and evaluate.
is 2 × 3, 6, 'builtin × still parses with a user infix:<×> declared';
is 12 ÷ 4, 3, 'builtin ÷ still parses with a user infix:<÷> declared';
is 2 × 3 + 1, 7, '× keeps multiplicative precedence (tighter than +)';

# The user candidates dispatch for the mixed operand types.
my $a = Angle.new(v => 5);
is ($a × 3).v, 15, 'user infix:<×>(Angle, Real) dispatches';
is (2 × $a).v, 10, 'user infix:<×>(Real, Angle) dispatches';
is ($a ÷ 5).v, 1, 'user infix:<÷>(Angle, Real) dispatches';
