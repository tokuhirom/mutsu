use Test;
use lib 't/lib';

# The BEGIN-time preload of a module only a block that never runs `use`s
# loads the module, but imports nothing: a `sub EXPORT` hook's routines and
# values are the block's, and the block never ran (ADR-8751, #14140).

plan 3;

if False { use HookExportsValues }

is (try EVAL 'hf()') // 'gone', 'gone', 'no hook routine is visible';
is (try EVAL '$hv') // 'gone', 'gone', 'no hook scalar is visible';
is (try EVAL 'ht') // 'gone', 'gone', 'no hook term is visible';
