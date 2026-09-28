use Test;

plan 4;

# `UInt` is a subset (`subset UInt of Int where * >= 0`), and a subset can
# never be `.new()`-ed at all -- "Cannot instantiate a subtype"
# (roast/S32-num/int.t, roast/6.c/S02-types/subset-6c.t). That prohibition
# used to leak the internal dispatcher's own wording instead of a normal
# exception ("Unknown method value dispatch (fallback disabled): new on
# UInt"), and the *same* unconditional `.new()` call inside the builtin
# `Rational[NuT]` prelude meant no class could ever compose `Rational[UInt]`
# -- a documented, legal parameterization (raku-doc Type/Rational.rakudoc)
# whose numerator/denominator are typed `UInt` but never need constructing
# via `.new`, only via ordinary parameter type-checking (#9795).
throws-like { UInt.new }, Exception, 'UInt.new still throws (it is a subset)';

class Rational9795Class does Rational[UInt] { }
is Rational9795Class.new(1, 3).Str, '0.333333',
    'a class does-ing Rational[UInt] constructs and stringifies correctly';

is Rational9795Class.new(2, 4).Str, '0.5',
    'construction still reduces numerator/denominator by their gcd with a UInt attribute';

throws-like { Rational9795Class.new(-1, 3) }, Exception,
    'a negative numerator is rejected by the UInt parameter constraint itself';
