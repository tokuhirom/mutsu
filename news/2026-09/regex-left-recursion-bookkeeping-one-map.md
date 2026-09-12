# The regex engine's left-recursion bookkeeping stops paying for three String-keyed tables

Every `<subrule>` reference the regex engine resolves registers a
*left-recursion activation* for the duration of its body walk, so that a
re-entry of the same rule at the same position reads a growing seed instead of
recursing forever. Almost no grammar is actually left-recursive — YAMLish is
not — but every subrule call at every position pays for the bookkeeping anyway.

On a 60-row YAML document that bookkeeping was **4.7% of the whole program**:
109.4 M instructions for 70,062 activations, about 1,560 instructions each, to
record three booleans' worth of state.

## What it cost

The state was three thread-local maps — `LR_MEMO` (the seed), `LR_ACTIVE` (is
it under evaluation), `LR_SEED_READ` (did anything read the seed) — each keyed
by an owned `(String, usize)` built from the rule name and the number of
characters remaining. So one call did:

- one `String` allocation to build the key,
- two more to insert it into `LR_MEMO` and `LR_ACTIVE`,
- three hash probes to arm the activation and three to tear it down, each
  hashing the rule-name bytes and confirming the hit with a `memcmp`.

In the callgrind profile that showed up as `lr_begin_activation` (56.4 M) and
`lr_end_activation` (53.0 M), and underneath them as the largest single caller
of `HashMap::remove`, a sixth of all `String::clone`s, and a share of the
allocator traffic that a self-cost profile reads as a diffuse tail.

## What it costs now

One map of one entry struct (`src/runtime/regex/regex_lr_state.rs`):

```rust
struct LrEntry {
    seed: Option<Vec<(usize, RegexCaptures)>>,  // Some == under evaluation
    seed_read: bool,
}
```

`seed: Option<_>` subsumes `LR_ACTIVE` (a key is active exactly while it holds a
seed), so every operation is one `entry()` lookup instead of three, and
`lr_end_activation` drops the entry once nothing is left to remember — which is
what keeps the map from accumulating one dead entry per (rule, position).

The key stops being a `String`. The rule name is now an interned `Symbol`, taken
from the `NamedRegexLookupSpec` that the engine already memoizes per `<subrule>`
atom text, so it is interned once per distinct atom in the program rather than
once per call. Hashing it is four bytes instead of the name, comparing it is an
integer compare instead of a `memcmp`, and cloning the key allocates nothing.

The rule *arguments* stay out of the symbol table on purpose. A parameterized
call's argument values are part of its rule identity — `multi rule expr($p)`
calling `<expr($p-1)>` at the same position is ordinary recursion toward a base
case, not left recursion — but they are runtime data, and interning them would
grow the (leaked, append-only) symbol table without bound. They live in a
separate `Option<Box<str>>` field, `None` in the overwhelmingly common case.
That also makes the key strictly more discriminating than the old one, which
joined name and arguments into a single NUL-separated string.

While the interned name was there, `resolve_parsed_token_candidates_in_pkg` —
the memoized subrule resolver, probed once per subrule reference at every
position — stopped re-interning it to build its own cache key and takes it from
the caller instead.

## Effect

Instructions retired on the 60-row document (callgrind `Ir`, deterministic and
load-independent):

| | Ir | vs. baseline |
| --- | ---: | ---: |
| before | 2,341,850,737 | |
| one map, interned key | 2,194,028,605 | -6.31% |
| + resolver takes the interned name | 2,178,208,714 | **-6.99%** |

`Symbol::intern` goes from 87.3 M (3.73%) to 71.6 M (3.29%) inclusive, and
`resolve_parsed_token_candidates_in_pkg` from 61.3 M to 45.2 M.

`t/regex/regex-left-recursion-key-identity.t` pins the three ways the key has to
discriminate (name, arguments, position) and that an activation erases itself
again; five of its seven rows were verified against `raku`, and the two
left-recursive ones have no oracle because Rakudo has no growing-seed loop and
hangs on a left-recursive rule. The unit tests in the new module pin the
seed-read handover and that a completed round trip leaves the map empty.

Part of [#7576](https://github.com/tokuhirom/mutsu/issues/7576) (round 14).
