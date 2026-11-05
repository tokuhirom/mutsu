use Test;

# Assigning a :D variable its own type object (what Nil resets an undefaulted
# container to) carries rakudo's "perhaps Nil" hint; any other wrong value does not.

plan 6;

my Int:D $d = 3;
try $d = Int;
is $!.message,
    'Type check failed in assignment to $d; expected Int:D but got Int (Int) (perhaps Nil was assigned to a :D which had no default?)',
    'the type object of the constraint gets the hint';
try $d = Nil;
like $!.message, /'perhaps Nil was assigned'/, 'Nil gets the hint';
try $d = Str;
is $!.message, 'Type check failed in assignment to $d; expected Int:D but got Str (Str)',
    'another type object does not';
try $d = "x";
is $!.message, 'Type check failed in assignment to $d; expected Int:D but got Str ("x")',
    'a defined wrong value does not';
my Int $plain = 3;
try $plain = "x";
unlike $!.message, /perhaps/, 'no hint on a plain typed scalar';
is $d, 3, 'the failed assignments left the value alone';
