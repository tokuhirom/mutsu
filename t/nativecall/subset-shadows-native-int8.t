use Test;
plan 4;

my subset int8 of Int where -128 <= $_ <= 127;
my int8 $c;
nok $c.defined, 'a user subset named int8 shadows the native type: no default 0';
is $c.raku, 'int8', 'the declared variable holds the subset type object';
my int16 $d;
is $d, 0, 'an unshadowed native width still defaults to 0';
is (my int32 $e), 0, 'int32 still defaults to 0';
