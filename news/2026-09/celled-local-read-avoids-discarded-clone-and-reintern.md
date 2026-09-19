# Celled local reads no longer clone twice or re-intern the variable name

`exec_get_local_op_inner`'s `ContainerRef` path — the one every `:=`-bound (celled) `my`
local takes on read — did two pieces of avoidable per-read work:

1. Its `HashEntryRef` deferred-token tag probe cloned the cell's inner `Value` just to
   answer a `matches!()` on its view, then threw that clone away in the common
   (non-`HashEntryRef`) case. `Value::into_deref()`, called a few lines below, already
   does the one clone this read actually needs — the first clone was pure waste.
2. The env-binding identity check (whether the slot's own cell is still the one `env`
   names under this key) called `Env::get(&str)`, which re-interns the variable name
   into a `Symbol` on every read — right next to the pre-interned `code.locals_sym`
   table the same function already consults ~40 lines below for exactly this reason.
   Switched it to `Env::get_for` with the pre-interned `Symbol`, mirroring the pattern
   already in use.

Both changes are behaviour-preserving by construction: the tag probe still resolves the
`HashEntryRef` case correctly (it just clones only when the tag actually matches), and
`Env::get_for` with a `Some(sym)` is defined to be `Env::get_sym(sym)`, which is what
`Env::get(name)` already reduced to.

## Measurement

Deterministic instruction count (`valgrind --tool=callgrind`) on the read-heavy repro from
[#8749](https://github.com/tokuhirom/mutsu/issues/8749) (two celled reads per iteration,
120,000 iterations, JIT off, release build):

| | Ir |
| --- | ---: |
| before | 556,151,244 |
| after | 521,347,267 |
| reduction | 34,803,977 (6.26%) |

Output (`checksum = 16120032`) is identical before and after.

Pinned by the existing `t/vm/writeback/` suite (65 files, 1000 assertions).

Closes #8749.
