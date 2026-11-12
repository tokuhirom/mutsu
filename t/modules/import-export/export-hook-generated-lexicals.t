use Test;

# A `sub EXPORT` may merge declarations into its importer at BEGIN time
# (#15371, CompUnit::Util's `set-unit` / `set-lexpad`): one merged into `$*CU`
# belongs to the importing unit, one merged into `$*R.current-scope` to the
# block that holds the `use`. The `use lib` below is a path chain, not a
# literal: it is still in force for the BEGIN-time load of the block's module.
# Checked against rakudo (RakuAST frontend).

plan 6;

use lib $?FILE.IO.parent(2).sibling('lib').Str;

{
    use GeneratedLexicals;
    is unit-term, 'from-unit', 'a declaration merged into $*CU is visible in the block';
    is pad-term, 'from-pad', 'one merged into the block scope too';
}

is unit-term, 'from-unit', 'the unit declaration outlives the block';
nok ::('pad-term'), 'the block declaration does not';

sub top-level-use() {
    use GeneratedLexicals;
    (unit-term, pad-term)
}
is-deeply top-level-use(), ('from-unit', 'from-pad'), 'a use in a routine body';
is unit-term, 'from-unit', 'and the unit keeps its own';
