use Test;
use experimental :rakuast;

# A BEGIN phaser written in a nested block sees that block as
# `$*R.current-scope`: one scope object for every BEGIN of the block, distinct
# from the unit's and from an inner block's (#17515). A BEGIN-time
# `resolve-lexical-constant(:current-scope-only)` there does not reach the
# unit's names. Checked against rakudo.

plan 6;

BEGIN my $unit = $*R.current-scope.WHICH;
BEGIN my $unit-again = $*R.current-scope.WHICH;
is $unit-again, $unit, 'the unit scope is one object';

{
    BEGIN my $outer = $*R.current-scope.WHICH;
    BEGIN my $outer-again = $*R.current-scope.WHICH;
    isnt $outer, $unit, "a nested block's BEGIN is in the block's scope";
    is $outer-again, $outer, 'every BEGIN of the block shares it';
    {
        BEGIN my $inner = $*R.current-scope.WHICH;
        isnt $inner, $outer, 'an inner block has a scope of its own';
    }
}

my package UnitPkg { }
BEGIN my $at-unit = $*R.resolve-lexical-constant('UnitPkg', :current-scope-only).defined;
ok $at-unit, 'the unit scope holds its package';
{
    BEGIN my $in-block = $*R.resolve-lexical-constant('UnitPkg', :current-scope-only).defined;
    nok $in-block, "a nested block's own scope does not";
}
