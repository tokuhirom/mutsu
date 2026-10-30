use Test;

plan 9;

my Int $pulls = 0;
my $nl = (lazy gather { loop { $pulls++; take 1 } }).List;

ok $nl ~~ Positional, 'a lazy list matches Positional without being read';
ok $nl ~~ List, 'a lazy list matches List without being read';
ok $nl ~~ Iterable, 'a lazy list matches Iterable without being read';
ok $nl ~~ Any, 'a lazy list matches Any without being read';
nok $nl ~~ Hash, 'a lazy list does not match Hash';
is $pulls, 0, 'no element was pulled by any type match';
ok $nl.is-lazy, 'the list is still lazy';

# A finite gather is still read whole when matched against a list value.
ok ((gather { take 1; take 2 }) ~~ (1, 2)), 'a finite gather matches a list by value';
nok ((gather { take 1; take 2 }) ~~ (1, 3)), 'and mismatches by value';
