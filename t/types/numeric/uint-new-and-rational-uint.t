use Test;

plan 3;

# `UInt` is a subset of `Int` (`subset UInt of Int where * >= 0`), and every
# other method call on a subset dispatches through its base type -- but
# `.new` fell through to the internal dispatcher's leaking fallback message
# instead ("Unknown method value dispatch (fallback disabled): new on UInt").
# This blocked any parametric role instantiated over `UInt` (e.g. the builtin
# `Rational[NuT]`) from ever constructing its typed attribute (#9795).
is UInt.new(5), 5, 'UInt.new delegates construction to its base type Int';

class Rational9795Class does Rational[UInt] { }
is Rational9795Class.new(1, 3).Str, '0.333333',
    'a class does-ing Rational[UInt] constructs and stringifies correctly';

is Rational9795Class.new(2, 4).Str, '0.5',
    'construction still reduces numerator/denominator by their gcd with a UInt attribute';
