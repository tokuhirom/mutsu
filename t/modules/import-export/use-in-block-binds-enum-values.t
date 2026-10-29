use Test;
use lib 't/lib';

# A block's `use` declares the values of an exported enum as terms of the
# block (ADR-8751, #14140), as it does an exported constant: no env alias is
# written for them, so there is nothing for the import scope to roll back.

plan 8;

sub outside { (try EVAL 'UibrtGreen') // 'none' }

my &c = do { use UseInBlockRerunTypes; -> { UibrtGreen.value } };
is c(), 1, 'a closure taken in the block keeps the enum value';

{
    use UseInBlockRerunTypes;
    is UibrtGreen.value, 1, 'the block sees the enum value';
    is UibrtRed.value, 0, 'and its other values';
    is outside(), 'none', 'a routine written outside the block does not see it';
}

is (try EVAL 'UibrtGreen') // 'gone', 'gone', 'the value is gone after the block';

# A module routine's bare name is its own type, not a term the caller bound
# (before: a constant exported under the name of the routine's class won).
use CallerConstWithError;
use OwnTypeOverCallerEnum;
is own-error-kind(), 'own class', "the module's own type wins over a caller constant";
is Error, 'error', "the caller's bare Error is still its constant";

{
    use CallerEnumWithError;
    is Debug.value, 'debug', 'an enum value with an Error sibling imports in a block';
}
