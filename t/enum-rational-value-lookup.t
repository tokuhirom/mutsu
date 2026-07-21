use Test;

plan 6;

# Calling an enum type as a coercer looks up the member by value. For a
# rational-valued enum the value is stored generically, and mutsu could not
# match a Rat (or an Int that equals it numerically) — `Mass(1/1000)` died with
# "No value '0.001' found in enum Mass". Look up by numeric/value equality.
enum Mass ( mg => 1/1000, g => 1/1, kg => 1000/1 );

is Mass(1/1000).key, 'mg', 'lookup by exact Rat value';
is Mass(1).key,      'g',  'Int matches a rational variant numerically';
is Mass(1000).key,   'kg', 'Int matches a larger rational variant';
is Mass(0.001).key,  'mg', 'Num matches a rational variant';

# Integer-valued enums are unaffected.
enum Color <red green blue>;
is Color(0).key, 'red',  'integer enum lookup by 0';
is Color(2).key, 'blue', 'integer enum lookup by 2';
