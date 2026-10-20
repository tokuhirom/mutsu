use Test;
use lib 't/lib';

# A routine written outside a block does not see the enum values the block's
# `use` imported (#14214, ADR-8751): they are the block's, as its imported
# variables, constants and types are.

plan 5;

sub outside { (try ::('UibrtGreen')) // 'none' }
sub outside-eval { (try EVAL 'UibrtGreen') // 'none' }

my &c = do { use UseInBlockRerunTypes; -> { UibrtGreen.value } };
is c(), 1, 'a closure taken in the block keeps the enum value';

{
    use UseInBlockRerunTypes;
    is UibrtGreen.value, 1, 'the block sees the enum value';
    is outside(), 'none', 'a routine written outside the block does not, by name';
    is outside-eval(), 'none', 'nor through an EVAL';
}

is (try EVAL 'UibrtGreen') // 'gone', 'gone', 'the value is gone after the block';
