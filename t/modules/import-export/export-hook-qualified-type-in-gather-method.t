use lib 't/lib';
use Test;

plan 4;

# A `sub EXPORT` hook returning package-qualified type keys: a routine of a
# module the hook loaded names those types by their qualified spelling, also
# when the routine keeps the caller's frame env (it holds a `gather`).
use HookSeedClasses;

is HookSeed::Hex.named-plain, 'HookSeed::Indexed', 'a plain method names the type';
is HookSeed::Hex.named-in-gather, 'HookSeed::Indexed', 'a method holding a gather names the type';
ok HookSeed::Hex.isa-in-gather, 'and the type still smart-matches its base';
is HookSeed::Indexed.^name, 'HookSeed::Indexed', 'the importing unit names the type';
