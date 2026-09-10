use v6;
use Test;

plan 8;

# .enums on an enum type object
enum Color <red green blue>;
is Color.enums.sort.gist, '(blue => 2 green => 1 red => 0)', 'Color.enums (type object)';

# .enums on an enum value (was: "No such method 'enums' for invocant of type 'Int'")
is red.enums.sort.gist, '(blue => 2 green => 1 red => 0)', 'red.enums (enum value)';
is green.enums.sort.gist, '(blue => 2 green => 1 red => 0)', 'green.enums (enum value)';

# .enums on the built-in Bool enum
is Bool.enums.sort.gist, '(False => 0 True => 1)', 'Bool.enums (type object)';
is True.enums.sort.gist, '(False => 0 True => 1)', 'True.enums (Bool value)';
is False.enums.sort.gist, '(False => 0 True => 1)', 'False.enums (Bool value)';

# The result is a Map
isa-ok Color.enums, Map, 'Color.enums returns a Map';
isa-ok red.enums, Map, 'red.enums returns a Map';
