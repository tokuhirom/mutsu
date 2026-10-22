use Test;

# Chained stores through an `is Hash` / `is Array` subclass root go into its
# backing storage, and `$a[i] := $x` on an `is Array` subclass binds the slot
# (found via PDF::COS::Dict / PDF::COS::Array, #12907).
plan 9;

class D is Hash { }
my $y = 'b';

my $d = D.new;
$d<k> = {};
$d<k><B> = 5;
is $d<k><B>, 5, 'nested assignment through an is-Hash root';
$d<k><C> := $y;
$y = 'c';
is $d<k><C>, 'c', 'nested bind through an is-Hash root sees later writes';
is $d<k>.keys.sort.join(','), 'B,C', 'earlier nested keys survive the bind';
ok $d ~~ D, 'the root is still the subclass instance';

my $in = D.new;
my $outer = D.new;
$outer<k> = $in;
$outer<k><B> := $y;
$y = 'd';
is $in<B>, 'd', 'nested bind into an is-Hash value is visible through the value';

class A is Array { }
my $a = A.new;
$a[0] := $y;
$y = 'e';
is $a[0], 'e', 'bind into an is-Array subclass slot';
$y = 'f';
is $a[0], 'f', 'and it keeps following the source';
is $a.elems, 1, 'no stray attribute created';
ok $a ~~ A, 'still the subclass instance';
