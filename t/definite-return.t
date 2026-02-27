use Test;

plan 5;

my sub return-two(--> 2) { 3 }
is return-two(), 2, 'definite return value overrides implicit final expression';

throws-like
    'my sub bad(--> Nil) { return 1 }',
    X::AdHoc,
    'return with a value is rejected for definite return specs';

my $sunk = False;
my sub return-empty(--> Empty) { 1, { ++$sunk; last } ... * }
is return-empty().elems, 0, 'definite Empty return yields an empty list';
ok $sunk, 'final expression still runs in sink context';

my $pointy = -> --> "done" { 42 };
is $pointy(), "done", 'pointy blocks accept definite return values';
