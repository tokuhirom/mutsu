use Test;

plan 8;

# A numeric coercion of an enum member acts on its underlying value. For a
# string-valued enum, `cool.Int` is `'42'.Int`. Previously mutsu fell through to
# a bogus "No such method 'Int' for invocant of type 'Int'" error.
enum Numbers ( cool => '42', almost-pi => '3.14' );

is cool.Int,       42,   '.Int coerces the string value';
is almost-pi.Int,  3,    '.Int truncates a fractional string value';
is cool.Real,      42,   '.Real coerces the string value';
is almost-pi.Real, 3.14, '.Real keeps the fractional value';
is cool.value,     '42', '.value returns the raw string';

# Integer-valued enums are unaffected.
enum Color <red green blue>;
is blue.Int,     2, 'integer enum .Int';
is blue.Numeric, 2, 'integer enum .Numeric';
is blue.value,   2, 'integer enum .value';
