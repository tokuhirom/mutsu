use Test;

# A plain `sub infix:<op>` of a built-in operator that a block-scoped `use`
# imports replaces the operator in that block only: a routine written outside
# the block does not see it, even when called from inside. From the Wheel
# distribution (t/03-lexical, t/07-power), which exports plain `sub infix:<+>`
# and friends. `infix:</>` is covered on purpose: its name contains the `/`
# that separates a multi candidate's key. Checked against rakudo.
plan 9;

use lib $?FILE.IO.parent.parent.parent.add('fixtures/plain-builtin-op-import/lib').Str;

sub outside-add($a, $b) { $a + $b }
sub outside-div($a, $b) { $a / $b }
sub outside-pow($a, $b) { $a ** $b }

{
    use PlainBuiltinOps;
    is 1 + 2, 42, 'the imported + applies in the block';
    is 6 / 2, 43, 'the imported / applies in the block';
    is 2 ** 3, 44, 'the imported ** applies in the block';
    is outside-add(1, 2), 3, 'a routine written outside keeps the native +';
    is outside-div(6, 2), 3, 'a routine written outside keeps the native /';
    is outside-pow(2, 3), 8, 'a routine written outside keeps the native **';
}

is 1 + 2, 3, 'native + after the block';
is 6 / 2, 3, 'native / after the block';
is 2 ** 3, 8, 'native ** after the block';
