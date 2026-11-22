use Test;
use lib 'roast/packages/Test-Helpers/lib';
use Test::Util;

# In rakudo the MONKEY-SEE-NO-EVAL pragma is the lexical `&MONKEY-SEE-NO-EVAL`:
# the check on a string interpolated into a regex calls it and goes by its
# answer. rakudo's `Test` exports `sub MONKEY-SEE-NO-EVAL() { 1 }`, so a file
# that does `use Test` may interpolate a string that holds a code block
# (roast S05-metasyntax/angle-brackets.t, #18740).

plan 5;

my $counter = 0;
my $subrule = '{$counter++; \'<alpha>\'}';
'abc' ~~ /<$subrule>/;
is $counter, 1, 'a code block in an interpolated string runs after `use Test`';

{
    no MONKEY-SEE-NO-EVAL;
    throws-like { 'abc' ~~ /<$subrule>/ }, X::SecurityPolicy,
        'an inner `no MONKEY-SEE-NO-EVAL` still refuses it';
}

is_run q[my $c = 0; my $s = '{$c++; \'<alpha>\'}'; 'abc' ~~ /<$s>/; print $c],
    { status => 1, out => '' }, 'without `use Test` the interpolation is refused';

is_run q[{ use Test; my $c = 0; my $s = '{$c++; \'<alpha>\'}'; 'abc' ~~ /<$s>/; print $c }],
    { status => 0, out => '1' }, 'a block-scoped `use Test` allows it inside the block';

is_run q[{ use Test; }; my $c = 0; my $s = '{$c++; \'<alpha>\'}'; 'abc' ~~ /<$s>/; print $c],
    { status => 1, out => '' }, 'but not after the block';
