use Test;

plan 6;

# Binding to a function call is illegal.
throws-like 'sub f() { }; f() := 2', X::Bind;
throws-like 'sub g($x) { }; g(1) := 2', X::Bind;

# Binding to a pseudo-package is illegal.
throws-like 'OUTER := 5', X::Bind, target => /OUTER/;
throws-like 'MY := 5', X::Bind, target => /MY/;

# Ordinary binds remain valid.
my $x;
$x := 2;
is $x, 2, 'scalar bind works';

my @a;
@a[0] := 9;
is @a[0], 9, 'array element bind works';
