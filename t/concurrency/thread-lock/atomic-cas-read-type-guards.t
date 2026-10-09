use Test;

plan 7;

my atomicint $counter = 1;
is cas($counter, 1, 2), 1, 'CAS returns the previous atomic value';
is $counter, 2, 'a local read sees the swapped atomic value';
dies-ok { cas($counter, 9, 'wrong type') }, 'type checking precedes a failed compare';
is $counter, 2, 'a rejected swap leaves the atomic value alone';

my $plain = 7;
is cas($plain, 7, 8), 7, 'a plain scalar can use the legacy CAS lane';
is $plain, 8, 'a plain scalar read sees the CAS result';
is cas($plain, 7, 9), 8, 'a failed compare returns the latest plain value';
