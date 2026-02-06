use Test;
plan 8;

is 42.WHAT, "(Int)", ".WHAT Int";
is "hello".WHAT, "(Str)", ".WHAT Str";
is True.WHAT, "(Bool)", ".WHAT Bool";
ok not False, "not operator";
ok so True, "so operator";
is elems([1, 2, 3]), 3, "elems function";
ok 7.is-prime, "7 is prime";
ok not 4.is-prime, "4 is not prime";
