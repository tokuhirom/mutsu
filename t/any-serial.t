use v6;
use Test;

# `.serial` returns the invocant's serial (non-parallel) form. For any
# ordinary value that is simply itself, so `.serial` is the identity — it
# previously threw "No such method 'serial'".

plan 11;

# The undefined default (Any) returns itself.
my $b;
is $b.serial.^name, 'Any', 'Any.serial is Any';
ok $b.serial === $b,       'undefined invocant returns itself';

# A defined scalar returns its value.
my $breakfast = 'food';
is $breakfast.serial, 'food', 'Str.serial is the string';

# Type object.
is Any.serial.^name, 'Any', 'Any type object serial';
is Int.serial.^name, 'Int', 'Int type object serial';

# Numbers and strings.
is 42.serial, 42,        'Int.serial';
is "str".serial, 'str',  'literal Str.serial';

# Containers keep their type and contents.
is (1, 2, 3).serial.raku, '(1, 2, 3)',   'List.serial';
is [1, 2, 3].serial.raku, '[1, 2, 3]',   'Array.serial';
is (1..5).serial.raku,    '1..5',        'Range.serial';
my %h = a => 1;
is %h.serial.raku, '{:a(1)}', 'Hash.serial';
