use Test;

# `//`, `||` and `&&` yield the selected operand's container, so assigning
# to the result writes to that operand (`//` binds tighter than `=`).

plan 7;

my $flag = False;
Nil // $flag = True;
ok $flag, '`Nil // $x = v` assigns to $x';

my $u;
my $z = 5;
$u // $z = 7;
is $z, 7, 'undefined left side: the right side is assigned';

my $d = 4;
$d // $z = 0;
is-deeply ($d, $z), (0, 7), 'defined left side: the left side is assigned';

($u || $z) = 9;
is $z, 9, '`||` picks the right side for a false left';

my $t = 1;
($t && $z) = 3;
is $z, 3, '`&&` picks the right side for a true left';

my $n = 0;
(try { $n++; die 'x' }) // $z = 11;
is-deeply ($n, $z), (1, 11), 'the left side runs once';

throws-like { (5 // $z) = 1 }, X::Assignment::RO, 'a picked non-container refuses';
