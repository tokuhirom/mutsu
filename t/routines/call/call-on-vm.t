use Test;
plan 3;

my $f = -> $x { $x + 1 };
is $f.(41), 42, 'call-on works for lambda target';

my $v = (sub ($x) { $x * 2 }).(21);
is $v, 42, 'call-on works for anon sub target';

sub add1($x) { $x + 1 }
is &add1.(4), 5, 'call-on with CodeVar target still works';
