use v6;
use Test;

# (B)-gate regression pin: atomic ops (`⚛$x`, `$x ⚛= v`, `$x⚛++`, `cas($x, …)`)
# are compiled to a `__mutsu_*_var(name_str, …)` builtin call that resolves the
# target VARIABLE by NAME from env (`atomic_current_value` falls back to
# `env.get(name)` for a non-`atomicint` scalar). Under MUTSU_GATE_LOCAL_ENV_WRITE
# a plain `my Int $x = 1` skips its env mirror (the slot is authoritative), so
# the builtin used to read the decl-seed type object. The emit sites now record
# the target local in `atomic_env_sync_locals`, folded into needs_env_sync under
# the gate. Must pass identically with the gate OFF (default) and ON.

plan 8;

# atomic fetch (⚛$x) on a plain Int scalar.
my Int $x = 1;
is ⚛$x, 1, 'atomic fetch reads the live scalar';

# atomic assign (⚛=) returns and updates.
is ($x ⚛= 2), 2, 'atomic assign returns assigned value';
is ⚛$x, 2, 'atomic assign updated the variable';

# cas 3-arg success path.
my Int $y = 5;
is cas($y, 5, 10), 5, 'cas returns the old value on success';
is ⚛$y, 10, 'cas swapped the value';

# cas 3-arg failed-compare path leaves the variable unchanged.
my Int $z = 7;
is cas($z, 999, 42), 7, 'cas returns current on failed compare';
is ⚛$z, 7, 'cas left the variable unchanged on failed compare';

# cross-thread atomicint increment still accumulates correctly (`⚛++` requires
# an atomicint; its value lives in the shared store, so folding it is harmless).
my atomicint $i = 0;
await start { for ^1000 { $i⚛++; } } xx 4;
is atomic-fetch($i), 4000, 'atomicint increment accumulates across start/await';
