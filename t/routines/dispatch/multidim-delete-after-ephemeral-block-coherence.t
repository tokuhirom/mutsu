use Test;
plan 2;

# Regression: `current_code` (the raw address of the currently-executing
# CompiledCode, read back by `writeback_multidim_var_to_local` after a
# multidim `:delete`) was not saved/restored around a nested, ephemeral
# block run (`with_nested_registers`, used by `dies-ok`/`lives-ok`/
# `throws-like`/EVAL). After such a block returned and its temporary
# CompiledCode was dropped, `current_code` was left dangling, and a later
# multidim `:delete` with a Whatever/negative index (which triggers the
# writeback) dereferenced it -- undefined behavior (SIGABRT: "slice::
# from_raw_parts requires the pointer to be aligned and non-null"),
# reproduced by roast/S32-array/multislice-6e.t.
dies-ok { die "warm up an ephemeral compiled block" },
  'an ephemeral block runs and returns first';

my @a = [[[42, 666, [314]], ], ];
is @a[*-1;0;0]:delete, 42,
  'multidim delete with a Whatever index after an ephemeral block does not crash';
