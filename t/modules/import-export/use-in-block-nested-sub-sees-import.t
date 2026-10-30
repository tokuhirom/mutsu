use Test;
use MONKEY-SEE-NO-EVAL;
use lib 't/lib';

# A named `sub` written inside a block that `use`s a module sees what the
# block imported, through every way of naming it, however it is called; a
# routine written outside the block still does not (#15661, ADR-8751).
# Checked against rakudo.
plan 8;

sub outside { (try ::('BgeType').hi) // 'none' }
sub outside-eval { (try EVAL 'BgeType.hi') // 'none' }

{
    use BlockGlobalExportType;
    sub direct { BgeType.hi }
    sub by-name { (try ::('BgeType').hi) // 'none' }
    sub by-eval { (try EVAL 'BgeType.hi') // 'none' }
    sub nested-block { { (try ::('BgeType').hi) // 'none' }() }
    my &closure = -> { (try ::('BgeType').hi) // 'none' };

    is direct(), 'bge', 'a nested sub names the imported type directly';
    is by-name(), 'bge', 'a nested sub finds it by name';
    is by-eval(), 'bge', 'a nested sub finds it in an EVAL';
    is nested-block(), 'bge', 'a block inside the nested sub finds it too';
    is closure(), 'bge', 'a closure finds it';
    is outside(), 'none', 'a routine written outside the block does not see it';
    is outside-eval(), 'none', '... nor in an EVAL';
}
is (try EVAL 'BgeType.hi') // 'gone', 'gone', 'the type is gone after the block';
