# Complexity annotations

Some hot built-in families carry one comment per operation stating its
per-call time complexity. The comment always has the same grep-able form:

```text
// Cost: O(n), n = chars of the invocant.
// Cost: O(n), n = chars of the invocant. Rakudo: O(1) -- see #NNNN.
```

Annotated families so far:

| family | where | reference suffix | measurement script |
|---|---|---|---|
| `nqp::` ops | `src/runtime/nqp_ops*.rs`, `src/runtime/nqp_pure.rs`, `src/compiler/nqp_forms.rs` | `MoarVM: O(..)` | `scripts/nqp-complexity-check.sh` |
| `Str` methods | wherever each method's body lives (see the list in `scripts/str-complexity-check.sh`) | `Rakudo: O(..)` | `scripts/str-complexity-check.sh` |
| `Array` / `List` / `Seq` operations | wherever each operation's body lives (see the list in `scripts/array-complexity-check.sh`) | `Rakudo: O(..)` | `scripts/array-complexity-check.sh` |

## Rules

- **What counts.** The bound is the body's own work per call. The fixed
  dispatch overhead every call pays (method resolution, the argument
  prologue, the dispatch `match`) is a constant and is not counted.
- **Variables.** Each comment names what its variables measure. The usual
  letters are:
  - `n`: length of the string or buffer operand, in chars or bytes as stated.
  - `e`: elements of an array or hash operand.
  - `k`: elements or chars produced or requested.
  - `m`: needle or separator length.
  - `r`: number of matches or replacements.
- **What is O(n) and what is O(1).**
  - Copying a `Str` payload is O(n). That includes
    `to_string_value()`, `to_string()` and `String::from`.
  - Cloning a `Value` is a refcount bump, O(1).
  - Mapping a char index to a byte offset of a UTF-8 `String` is O(n)
    unless the code has a shortcut, such as an ASCII check or a cached
    index. Rakudo keeps strings as fixed-width grapheme arrays, so `chars`
    and `substr` positioning are O(1) there.
- **Amortized.** A cost written as "amortized" states what it relies on
  (a cache hit, spare `Vec` capacity, ...).
- **Deficit suffix.** The reference suffix (`MoarVM:` / `Rakudo:`) appears
  only where mutsu's bound is worse than the reference implementation's for
  the same operation. Every such suffix names a tracking issue, so
  `grep -rn 'MoarVM: O(\|Rakudo: O(' src/` lists every known complexity
  deficit.
- **Fixing a deficit.**
  1. Re-run the family's measurement script on the case. Each case times a
     loop at N and at 2N; a time ratio near 2 means the loop is linear, and
     near 4 means it is quadratic.
  2. Drop the suffix and close the issue in the same PR.
- **The scripts are manual diagnostics, not CI gates.** Wall-clock ratios
  move with machine load. A regression guard belongs in a deterministic test,
  for example a counter that `MUTSU_VM_STATS` exposes.
