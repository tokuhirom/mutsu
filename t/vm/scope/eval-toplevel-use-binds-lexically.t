use Test;
use lib 't/lib';

# An `EVAL`'s own top-level `use` is lexical to the EVAL'd unit (ADR-8751,
# #14140): a closure the EVAL returns keeps the import, nothing is left
# visible after the EVAL, and a hook's exported variable is declared in it.

plan 6;

sub hf { 'outer-hf' }

is EVAL(q[use HookExportsValues; hf() ~ ' ' ~ $hv ~ ' ' ~ ht]), 'hf hv-val term-val',
    'the EVAL sees what its use imported';
my &k = EVAL q[use HookExportsValues; -> { hf() }];
is k(), 'hf', 'a closure the EVAL returns keeps the imported routine';
is hf(), 'outer-hf', 'after the EVAL the name is the program\'s routine again';
is (try EVAL '$hv') // 'gone', 'gone', 'the hook scalar is gone after the EVAL';

is EVAL(q[use EvalUseVarFixture; "$vx " ~ vk]), 'vx-val vk-val', 'an is-export variable and constant';
is (try EVAL '$vx') // 'gone', 'gone', 'gone after the EVAL';
