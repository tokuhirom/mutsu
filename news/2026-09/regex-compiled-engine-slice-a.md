# The first compiled regex engine: 8-15x on the scans no prefilter can help

ADR-0135 decided that a regex compiles to a flat instruction program run by one backtracking
loop, and that the tree walk is retired once that loop covers the whole language. The first
part of its Slice A has landed ([#10251](https://github.com/tokuhirom/mutsu/issues/10251)).

`src/runtime/regex/rx/` compiles a pattern to an `RxProgram`, a vector of `RxOp`s, once per
pattern, memoized in its `PatternDerived`. The program runs in one loop, with an explicit stack
of choice points and the walk's own `CapStore` trail for captures. The walk's first-match
chokepoint consults it first, so `~~`, the prefiltered unanchored scan, `:g`, `.subst` and
`.split` all take it when the pattern is covered. The slice covers:

- one-grapheme atoms and zero-width assertions;
- groups, and capture groups whose body captures nothing;
- `$<x>=` and `$N=` aliases;
- greedy, frugal, ratcheted and counted quantifiers.

Any other pattern keeps the walk, with the reason on the `regex-vm:` line of `MUTSU_VM_STATS`.

Two rules from the ADR shaped the code.

**Single definition.** Nothing in the new engine restates what an atom matches.
- A one-grapheme atom is tested by `match_consuming_atom` and an assertion by
  `regex_match_atom_in_pkg`, the same functions the walk calls.
- Captures go through the walk's own `capture_group_delta` and `store_apply_named_capture`.
- Even the ASCII fast path is derived rather than written: each atom is asked once, through
  `match_consuming_atom`, about every printable ASCII character. The answer is consulted only
  where the grapheme at the cursor is provably that single character.

**Two engines are held to one answer.** `MUTSU_RX_DIFF=1` re-runs every compiled match through
the walk and aborts on any difference in the match, its end or a capture span. It agreed on
every file of `t/regex/`, `t/grammar/` and the whitelisted `roast/S05-*`.
`tests/regex_vm_differential.rs` runs a corpus with the engine on, with it off
(`MUTSU_RX_VM=off`), and under the differential mode.

The first version, one op per atom per iteration, managed only 2-3x. Callgrind put 38% of the
remaining cost in the generic atom test and 25% in per-start-position allocation. Three changes
followed:

- a greedy run over one atom became a single `AtomRun` op, which scans ahead and gives back from
  a position list;
- the ASCII fast path;
- reusing the VM's buffers across start positions.

The kill criterion the ADR set for this slice was at least 5x on its §2.3 rows (release, 640 KB):

| row | walk | compiled |
|---|---:|---:|
| failing `\w+ \s \d ** 6` | 370 ms | **45 ms** |
| failing `[ \w+ \s ] ** 3 \d ** 6` | 1,330 ms | **91 ms** |

That is 8.2x and 14.6x. Rakudo takes 385 and 1,320 ms on the same rows.
