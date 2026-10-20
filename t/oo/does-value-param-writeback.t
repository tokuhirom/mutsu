use Test;

# ADR-14144: `$v does R` on a readonly parameter holding a value type reaches
# the caller's container, as Rakudo's rebless of the shared object does.

plan 10;

my role MyRole {};

sub co($v, MyRole) { $v does MyRole }

my @a = 42, 10;
co(@a[1], MyRole);
ok @a[1] ~~ MyRole, 'array element passed to a readonly parameter';
nok @a[0] ~~ MyRole, 'a sibling element is untouched';

my $x = 10;
sub co2($v) { $v does MyRole }
co2($x);
ok $x ~~ MyRole, 'scalar variable';

my %h = a => 1;
co2(%h<a>);
ok %h<a> ~~ MyRole, 'hash element';

my $s = 'str';
co2($s);
ok $s ~~ MyRole, 'Str value';

class K {
    has $.a is rw = 3;
    method m($v) { $v does MyRole }
}
my $k = K.new;
K.new.m($k.a);
pass 'method parameter with a non-container source does not die';

sub ro($v) { $v = 5 }
my $r = 1;
try { ro($r) };
is $r, 1, 'the parameter stays readonly';

lives-ok { co2(10) }, 'a literal argument has no container to update';

# Documented divergence (ADR-14144): a copy taken before the call keeps the
# plain value, where Rakudo's shared box makes it do the role too.
my $y = 10;
my $copy = $y;
co2($y);
nok $copy ~~ MyRole, 'a copy taken before the call keeps the plain value';

my $o = class { has $.z }.new;
co2($o);
ok $o ~~ MyRole, 'an instance is still shared by reference';
