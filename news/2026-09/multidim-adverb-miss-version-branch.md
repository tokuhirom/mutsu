# A multidim `:v`/`:k`/`:p` miss branches on the language version

`todo/tickets/multidim-oob-coordinate-nil-vs-empty-list-version-pragma.md` was
filed as a *deliberate divergence, not a bug*: mutsu answered `Nil` where plain
(6.d) `raku` answers `()` for a shaped array's out-of-range multidim coordinate.

```
raku  -e 'my @a[2;2]; @a[0;0]=1; say (@a[5;5]:v).raku'   # ()
mutsu -e 'my @a[2;2]; @a[0;0]=1; say (@a[5;5]:v).raku'   # Nil   (before)
```

The ticket explained why one answer had to serve both: `roast/S32-array/
multislice-6e.t` and `roast/S32-hash/multislice-6e.t` (whitelisted, CI-gating,
both `use v6.e.PREVIEW`) pin `Nil`, and mutsu did not branch multidim-adverb
behaviour on the version pragma — so roast won. It closed with an explicit
condition for revisiting: *"per-version branching for multidim semantics becomes
needed for an unrelated reason (at which point this narrow case would be folded
in for free)"*. That happened the same day: the associative multislice
(`news/2026-09/associative-multidim-subscript.md`) introduced exactly that
branch, consulting `current_language_version()` from the VM.

## The rule is simpler than the ticket assumed

The ticket framed the split as being about the *kind* of miss — an in-bounds
Array hole (which carries its own non-`Nil` hole marker) versus an out-of-range
coordinate or a missing Hash key (a bare `Value::NIL`). Measuring all three
against `raku` under both versions shows the kind does not matter at all:

| miss | 6.d | 6.e |
| --- | --- | --- |
| in-bounds Array hole | `()` | `Nil` |
| out-of-range coordinate | `()` | `Nil` |
| missing Hash key | `()` (no candidate) | `Nil` |

So `multidim_missing_result` lost its `raw_value` argument entirely and is now a
pure version branch. `:kv` is unaffected — it is `()` for every miss under every
version.

## Pins

- `t/typed-array-hole-adverbs.t` (no pragma, so 6.d) had asserted `Nil` for the
  out-of-range block with a comment pointing at the ticket; it now asserts `()`,
  and passes under `raku` as well as mutsu.
- `t/multidim-adverb-miss-6e.t` is new and pins the 6.e side across all three
  kinds of miss plus `:kv` and `:!v`.
- The whitelisted `roast/S32-{array,hash}/multislice-6e.t` keep passing — they
  are the 6.e side, which is unchanged.
