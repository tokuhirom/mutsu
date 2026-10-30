use Test;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib';

# A named `sub` written inside a block sees what the block imported by every
# kind of import (`need`, `import`), not only
# the `use` of a lexically importable module; a routine written outside the
# block does not, and the import is gone after the block (#14140, ADR-14140,
# ADR-8751). Checked against rakudo.
plan 6;

sub outside-need { (try ::('BgeType').hi) // 'none' }

module Mod { our sub imp-f is export { 'imp' } }

{
    need BlockGlobalExportType;
    sub in-need { (try ::('BgeType').hi) // 'none' }
    is in-need(), 'bge', 'a sub written in a `need` block finds the needed type';
    is outside-need(), 'none', 'a sub written outside the block does not';
}
is (try EVAL 'BgeType.hi') // 'gone', 'gone', 'the needed type is gone after the block';

{
    import Mod;
    sub in-import { imp-f() }
    sub in-import-eval { (try EVAL 'imp-f()') // 'none' }
    is in-import(), 'imp', 'a sub written in an `import` block calls the imported routine';
    is in-import-eval(), 'imp', '... and in an EVAL';
}
is (try EVAL 'imp-f()') // 'gone', 'gone', 'the imported routine is gone after the block';
