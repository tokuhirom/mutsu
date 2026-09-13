use Test;

# ADR-0071: a natively-implemented infix is a *candidate* of `&infix:<op>`, so a
# user `multi` that out-narrows the core set takes the call. The word-named
# arithmetic operators never asked -- `div`, `mod`, `gcd`, `lcm`, `min` and `max`
# went straight to their opcode handler, so a user candidate for them was
# unreachable. Math::NumberTheory overloads `gcd`/`lcm` for Gaussian integers and
# Rationals; mutsu answered the integer core result for both (issue #7995).

plan 14;

class Vec2 { has ($.x, $.y) }

multi sub infix:<gcd>(Vec2:D $a, Vec2:D $b) { 'gcd-vec' }
multi sub infix:<lcm>(Vec2:D $a, Vec2:D $b) { 'lcm-vec' }
multi sub infix:<div>(Vec2:D $a, Vec2:D $b) { 'div-vec' }
multi sub infix:<mod>(Vec2:D $a, Vec2:D $b) { 'mod-vec' }
multi sub infix:<min>(Vec2:D $a, Vec2:D $b) { 'min-vec' }
multi sub infix:<max>(Vec2:D $a, Vec2:D $b) { 'max-vec' }

my $p = Vec2.new(x => 1, y => 2);
my $q = Vec2.new(x => 3, y => 4);

is $p gcd $q, 'gcd-vec', 'user infix:<gcd> takes the call for its own types';
is $p lcm $q, 'lcm-vec', 'user infix:<lcm> takes the call for its own types';
is $p div $q, 'div-vec', 'user infix:<div> takes the call for its own types';
is $p mod $q, 'mod-vec', 'user infix:<mod> takes the call for its own types';
is $p min $q, 'min-vec', 'user infix:<min> takes the call for its own types';
is $p max $q, 'max-vec', 'user infix:<max> takes the call for its own types';

# The core candidate still answers everything the user's does not accept.
is 12 gcd 18, 6, 'core gcd still answers Int operands';
is 4 lcm 6, 12, 'core lcm still answers Int operands';
is 7 div 2, 3, 'core div still answers Int operands';
is 7 mod 3, 1, 'core mod still answers Int operands';
is 3 min 5, 3, 'core min still answers Int operands';
is 3 max 5, 5, 'core max still answers Int operands';

# ... including through a variable, and with the reduction metaoperator.
my $a = 24;
my $b = 36;
is $a gcd $b, 12, 'core gcd answers variable operands';
is ([gcd] 24, 36, 60), 12, 'core gcd answers under the reduction metaoperator';
