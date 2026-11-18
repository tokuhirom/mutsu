use Test;
use lib 't/lib-unicode-constant-export';

# A `unit module`'s exported constant whose name starts with a non-ASCII
# letter (`my constant \ℝ is export`) is imported like an ASCII one; the
# module-lexical collection only took names starting with an ASCII letter
# (Math::Sequences::Real's `ℝ`).

plan 4;

use UniConst;

is Ñ, 42, 'a constant named with a non-ASCII initial';
isa-ok ℝ, Reals, 'a sigilless one';
is ℝ.elems, Inf, 'its class overrides elems';
is plain, 7, 'an ASCII one still imports';
