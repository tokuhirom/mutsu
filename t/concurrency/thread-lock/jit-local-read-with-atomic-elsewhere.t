use Test;

# The JIT's inline local read asks the slot's own name whether an atomic
# variable of that name exists, not whether any atomic exists in the process
# (#7817). A routine compiled before an `atomicint` of the same name is
# declared still reads its own plain variable afterwards, and an unrelated
# variable keeps reading correctly beside the atomic.
plan 6;

sub hot($n) { my $t = 0; my $u = 0; for ^$n { $t = $t + $_; $u = $u + 1 }; ($t, $u) }

is-deeply hot(3000), (4498500, 3000), 'a hot loop compiled before any atomic exists';

my atomicint $t = 0;
$t⚛++ for ^10;
is $t, 10, 'the atomic counts';

is-deeply hot(3000), (4498500, 3000),
    'the routine\'s own $t is still its own plain variable after an atomic $t exists';

my $x = 5;
my $sum = 0;
for ^3000 { $sum = $sum + $x }
is $sum, 15000, 'an unrelated variable beside the atomic';

my atomicint $late = 0;
sub reads-late($n) { my $late = 0; for ^$n { $late = $late + 2 }; $late }
$late⚛++;
is reads-late(3000), 6000, 'a routine declaring a plain variable named like a later atomic';
is $late, 1, 'and the atomic is untouched by it';
