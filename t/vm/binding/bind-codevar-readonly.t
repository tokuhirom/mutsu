use Test;
plan 6;

my &bound := { 1 };
my $error;
try { &bound = { 2 }; CATCH { default { $error = $_ } } }
is $error.^name, 'X::Assignment::RO', 'a value-bound code variable raises the value assignment error';
is &bound(), 1, 'a failed assignment leaves the code binding intact';

my &assigned = { 1 };
&assigned = { 2 };
is &assigned(), 2, 'an assigned code variable stays writable';

my &writer = { &bound = { 3 } };
$error = Nil;
try { writer(); CATCH { default { $error = $_ } } }
is $error.^name, 'X::Assignment::RO', 'a closure sees the readonly code binding';
is &bound(), 1, 'a failed closure assignment leaves the binding intact';

my &infix:<readonly-op> := { $^a + $^b };
is &infix:<readonly-op>(2, 3), 5,
    'a value-bound operator remains callable by its code variable name';
