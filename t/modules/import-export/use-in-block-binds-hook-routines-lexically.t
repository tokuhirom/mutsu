use Test;
use lib 't/lib';

# A plain routine a `sub EXPORT` hook returns is bound only in the importing
# block's lexical import set, with no env alias (ADR-8751, #14140): every
# use in the block reads the set, a closure keeps it, and neither a routine
# written outside the block nor the code after it sees it.

plan 9;

sub hf { 'outer-hf' }
sub outside { hf() }

my &c = do { use HookExportsValues; -> { hf() } };
is c(), 'hf', 'a closure taken in the block keeps the routine';

{
    use HookExportsValues;
    is hf(), 'hf', 'a call in the block reaches the import';
    is &hf(), 'hf', 'so does a call through &hf';
    is (1, 2).map({ hf() }).join(','), 'hf,hf', 'and one in a block it passes on';
    is ::('&hf')(), 'hf', 'a by-name lookup in the block finds it';
    is EVAL('hf()'), 'hf', 'an EVAL in the block finds it';
    is outside(), 'outer-hf', 'a routine written outside the block gets its own';
}

is hf(), 'outer-hf', 'after the block the name is the program\'s routine again';

if False { use HookExportsValues }
is hf(), 'outer-hf', 'the BEGIN-time preload of a block-only module imports no routine';
