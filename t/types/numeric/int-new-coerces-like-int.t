use Test;

# `Int.new(value)` coerces as `value.Int` does: a numeric string parses the
# way Raku reads one, a string that is not a number dies (it answered 0), and
# a type object is refused by name.

plan 7;

is Int.new("42"), 42, 'a decimal string';
is Int.new("0x1F"), 31, 'a radix-prefixed string';
is Int.new("3.7"), 3, 'a fractional string truncates';
is Int.new(4.7), 4, 'a Rat truncates';
is Int.new, 0, 'no argument is 0';
throws-like { Int.new("x") }, X::Str::Numeric, 'a non-numeric string dies';
throws-like { Int.new(Any) }, Exception,
    message => "Cannot create an Int from a 'Any' type object", 'a type object';
