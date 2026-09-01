# Natively constructed hashes bypass ADR-0040's element itemization

A `Hash` that a native Rust builtin builds directly — rather than one a Raku
program assigns or a literal constructs — stores its values **bare**, because
none of ADR-0040's store hooks are on that path. Two compensators
(`itemize_hash_value` on the hash-subscript read, `raku_hash_value` in the
renderer) still paper over it, which is the only thing keeping ADR-0040 slice 4
from deleting them.

## Measured (2026-09-01, whole corpus, instrumented)

Both compensator sites were instrumented behind `MUTSU_COMP_PROBE` and the
entire `t/` suite (3601 files) plus the full roast whitelist (1425 files) were
run. After slice 4's chained-subscript store fix
(`news/2026-09/adr0040-slice4-chained-subscript-store.md`):

- `raku_hash_value` (render side): **0** firings in `t/`, 1 in roast — a
  self-referential hash, where what gets rendered is the cycle sentinel
  (`:__mutsu_self_hash_ref`) rather than a stored container. That one is
  arguably not this ticket.
- `itemize_hash_value` (read side): 3 in `t/`, 17 in roast, and **every one is a
  natively constructed hash**:

| source | firings | value |
| --- | --- | --- |
| Pod block `.config` (`roast/S26-documentation/09-configuration.t`) | 12 | `(1, "b c", 2.3e0, Bool::True, Bool::False)` |
| `gethost(…)<addrs>` (`t/os-functions.t`) | 1 | `("127.0.0.1",)` |
| an exception group hash (`t/exceptions-comp-group-unknown-parent.t`) | 1 | `("Exception",)` |
| a `Proc`-shaped hash | 1 | `("…/mutsu", "-e", " ")` |
| others | 4 | `("uc",)`, `()` |

## Why it matters

It is the same class slice 2 already hit once and fixed by hand: mutsu's native
`JSON::Fast` provider built `Hash`/`Array` directly and so bypassed every hook,
which slice 2 patched at `Parser::finish_object` / `finish_array`
(`src/runtime/json.rs`). Pod `.config`, `gethost` and their siblings are the
rest of the same set.

The user-visible consequence is the ADR's own "one value, three answers" shape:
the compensators cover the hash-subscript read and the whole-hash render, but
not `.values`, `.pairs`, `.kv`, `.head`, a slice, or iteration — so those
readers disagree with `%h<k>` about the same value.

## What to do

Enumerate the native hash-construction sites and route their values through
`hash_stored_value` (the slice-2 helper, `decay_nil_hash_value` composed with
`itemize_for_element_store`), then delete both compensators and re-run the
instrumented sweep to confirm zero firings. `ArrayData`-building native sites
need the same treatment for the array half.

Start from the four named above; the instrumented sweep is the way to find the
rest (see the ADR's slice-4 section for the recipe, including the trap that
`raku_hash_value` has a second, non-compensator caller that must not be probed).

## Repro

```
my %h = a => (1,2);  say %h<a>.raku;   # $(1, 2)  -- assigned, itemized
# a Pod block's .config, or gethost("127.0.0.1")<addrs>, stores the same shape bare
```

Related: `docs/adr/0040-array-hash-elements-are-itemized-at-the-store.md` §8
(slice 2's JSON note, and slice 4's measurement table).
