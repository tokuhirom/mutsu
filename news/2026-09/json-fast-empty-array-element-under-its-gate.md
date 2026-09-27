# A JSON::Fast `[]` element is under ADR-0112's 5 µs gate

#9122 asked for one `[]` element of `from-json` to cost at most 5 µs, measured
as the n = 4001 run of its repro minus the n = 1 run, divided by 4000. Earlier
slices (#9154, #9179, #9185 and the ADR-0121 attribute work) had brought it to
~2.9 µs on `main` by that measure. This change takes it to ~2.3 µs, and closes
the issue. Callgrind puts one element at ~20.0K instructions, down from ~24.9K
on the same `main`.

## What an element still paid for nothing

- **`bindattr` of a fresh buffer.** `parse-array` installs a buffer fresh from
  `nqp::create(IterationBuffer)` as `@result`'s `$!reified`. The bind vivified
  an empty store for the buffer, copied it into the array, then re-pointed the
  buffer at the array and dropped the store. A buffer with no store now skips
  straight to the re-point, which leaves the same state behind.
- **String attribute keys.** The buffer's storage key and the Buf storage key
  were `&str`s, so every `nqp::push` onto a buffer looked two names up in the
  interner. Both are interned once now.
- **SipHash on every routine frame.** Pushing a routine frame looks the
  declaring file up in three `Symbol`-keyed maps (`unit_module_packages`,
  `module_source_packages`, the EVAL parent table), and compared the file with
  `$*PROGRAM` by resolving the symbol back to text. The maps are `FxHashMap`s
  now, and the program path is kept as a symbol beside its string. The
  `CallGen` link table moved to `FxHashMap` too.
- **The grapheme index of the text being scanned.** Every `nqp::ordat` fetched
  the text's grapheme index from a thread-local cache, and moved the hit to
  the front even when it was already there. TRIR now keeps the last text's
  index in a one-entry memo on its own stacks, dropped when the outermost TRIR
  frame returns, and the cache skips the move for a front hit. `OrdAt*` and
  `CharsLocal` also read their operand in place rather than cloning it. The
  cache hands out `Arc`s instead of `Rc`s, because the interpreter the memo
  lives in has to be `Send`.

## Measured

Release build, 4-core container, #9122's repro, 31 paired runs per binary, each
binary's runs back to back so the module precomp cache stays warm (alternating
binaries recompiles the module on every switch):

| | n = 1 | n = 4001 | per element |
|---|---:|---:|---:|
| `main` (`2179535a`) | 25.0 ms | 36.7 ms | 2.92 µs |
| this change | 25.2 ms | 34.2 ms | 2.26 µs |

Rakudo's per-element cost, measured the same way on the same box, is noisier
because its startup dominates both runs; inside one process it decodes a `[]`
element in ~0.9 µs. The test for the new paths is
`t/vm/codegen/adr0112-trir-container-edges.t`, which runs its fixture with
TRIR on and off and checks both against rakudo's transcript.
