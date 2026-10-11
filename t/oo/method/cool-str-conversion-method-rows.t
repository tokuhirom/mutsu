use v6.e;
use Test;

plan 54;

# Str.Numeric, Str.Int, Str.Num
is-deeply "42".Numeric, 42, 'Str.Numeric of an integer string';
is-deeply "1.5".Numeric, 1.5, 'Str.Numeric of a decimal is a Rat';
is-deeply "1e3".Numeric, 1e3, 'Str.Numeric of an exponent string is a Num';
is-deeply " 7 ".Numeric, 7, 'Str.Numeric trims the string';
is-deeply "1/2".Numeric, 1/2, 'Str.Numeric of a rational string';
is-deeply "1+2i".Numeric, 1+2i, 'Str.Numeric of a complex string is the Complex';
is-deeply "0x1F".Numeric, 31, 'Str.Numeric reads a radix prefix';
is-deeply "42".Int, 42, 'Str.Int';
is-deeply "".Int, 0, 'an empty string is 0';
is-deeply "-7".Int, -7, 'a negative integer string';
is-deeply "1.9".Int, 1, 'Str.Int truncates';
is-deeply ":16<ff>".Int, 255, 'Str.Int reads the radix form';
is-deeply "123456789012345678901234567890".Int, 123456789012345678901234567890, 'a big integer string';
is-deeply "42".Num, 42e0, 'Str.Num';
is-deeply "".Num, 0e0, 'an empty string is 0e0';
is-deeply "-0".Num, -0e0, '"-0".Num keeps the sign of zero';
is-deeply "1/2".Num, 0.5e0, 'a rational string as a Num';
is-deeply "Inf".Num, Inf, 'Str.Num of Inf';
isa-ok "abc".Int, Failure, 'a string that is no number is a Failure';
isa-ok "abc".Num, Failure, '.Num of it too';
isa-ok "abc".Numeric, Failure, '.Numeric of it too';
is "abc".Int.exception.^name, 'X::Str::Numeric', '... an X::Str::Numeric';
"abc".Int.so;
is-deeply "1+0i".Int, 1, 'a complex string is coerced as the Complex (Complex.Int)';
throws-like { "1+2i".Int }, X::Numeric::Real, 'a complex string with an imaginary part is not a Real';

# Cool.Int, Cool.Num, Cool.Rat, Cool.UInt of the aggregates and Str
is-deeply (1, 2, 3).Int, 3, 'List.Int is the element count';
is-deeply [1, 2].Int, 2, 'Array.Int';
is-deeply {a => 1, b => 2}.Int, 2, 'Hash.Int';
is-deeply (1, 2, 3).Num, 3e0, 'List.Num';
is-deeply [1, 2].Rat, 2.0, 'Array.Rat';
is-deeply "1.5".Rat, 1.5, 'Str.Rat';
is-deeply "3".Rat, 3.0, 'Str.Rat of an integer string';
isa-ok "x".Rat, Failure, 'Str.Rat of a non-number is a Failure';
is-deeply (1, 2, 3).UInt, 3, 'List.UInt';
is-deeply "5".UInt, 5, 'Str.UInt';
isa-ok "-5".UInt, Failure, 'a negative UInt is a Failure';
is "-5".UInt.exception.^name, 'X::OutOfRange', '... an X::OutOfRange';
"-5".UInt.so;

# Version
is-deeply "1.2.3".Version, v1.2.3, 'Str.Version';
is-deeply 5.Version, v5, 'Int.Version';
is-deeply 1.5.Version, v1.5, 'Rat.Version';
is-deeply (v1.2).Version, v1.2, 'Version.Version is itself';
is-deeply (1, 2).Version, v1.2, 'List.Version is Version.new(self.Str)';

# Capture of the scalars that have none
for 42, 2.5e0, "abc", v1.2.3, True -> $value {
    throws-like { $value.Capture }, X::Cannot::Capture, "{$value.raku}.Capture throws";
}
{
    CATCH { default { like .message, /'Cannot unpack or Capture `abc`.'/, 'the message names the value'; } }
    "abc".Capture;
}
is-deeply 3.75.Capture, \(:denominator(4), :numerator(15)), 'Rat.Capture keeps its attributes';

# Mu.Numeric / Mu.Int are the last candidate of a user override
class WithNumeric { method Numeric { callsame } }
throws-like { WithNumeric.new.Numeric }, X::Multi::NoMatch, 'callsame from a user Numeric reaches Mu.Numeric';
class WithInt { method Int { nextsame } }
throws-like { WithInt.new.Int }, X::Multi::NoMatch, 'nextsame from a user Int reaches Mu.Int';

# Bool.pick / Bool.roll are for the type object
ok Bool.pick ~~ Bool, 'Bool.pick is a Bool';
is-deeply Bool.pick(*).sort.List, (False, True), 'Bool.pick(*) is both values';
is Bool.roll(3).elems, 3, 'Bool.roll(3)';
throws-like { True.pick }, X::Multi::NoMatch, 'a defined Bool has no pick';
