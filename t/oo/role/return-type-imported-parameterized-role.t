use lib 't/lib';
use Test;
use ReturnTypeImportedParamRole;

# A `-->` return type written with an imported short name of a parameterized
# role (`--> Maybe[Int]`, where the module declared
# `ReturnTypeImportedParamRole::Maybe`) accepts a value mixing that role in,
# as the smartmatch already did. It used to die "Type check failed for return
# value; expected Maybe[Int]" (Definitely's `halve`).

plan 4;

my $v = something(2);
ok $v ~~ Maybe[Int], 'smartmatch against the imported parameterized role (baseline)';

sub halve(Int $x --> Maybe[Int]) { something($x div 2) }
lives-ok { halve(4) }, 'the return type accepts the mixed-in value';
is halve(4).value, 2, 'and returns it unchanged';

sub wrong(--> Maybe[Str]) { something(1) }
dies-ok { wrong() }, 'a different parameterization is still rejected';
