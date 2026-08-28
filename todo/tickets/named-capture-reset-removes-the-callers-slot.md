# A routine's named-capture reset deletes the CALLER's `$<name>` slot

`$/` and the numeric capture variables are now scoped per routine
(`runtime::utils::is_routine_scoped_implicit_var`, see
`news/2026-08/match-vars-are-routine-scoped.md`), so a routine that matches
internally no longer clobbers its caller's `$/`, `$0` or `$1`. A **named**
capture still leaks, and it leaks by *deletion* rather than by overwrite.

## Minimal repro

```raku
sub inner-match() { "zz" ~~ /(z)/; 1 }
"abc" ~~ /$<first>=(b)(c)/;
say ~$<first>;      # b
inner-match();
say ~$<first>;      # mutsu: '' (empty)   rakudo: b
```

`inner-match` has no named captures of its own, so nothing should touch
`$<first>` at all.

## Root cause (as far as diagnosed — verify before trusting it)

`Interpreter::reset_capture_env_vars`
(`src/runtime/seq_helpers/regex_captures.rs`) clears stale capture slots before
each match. Numeric slots are *set to `Nil`*, but angle-bracket slots are
**removed**:

```rust
for key in angle_keys {
    self.env.remove_sym(key);
}
```

The doc comment above it explains why removal rather than Nil-ing is
load-bearing there (a present-but-`Nil` entry would shadow the local-slot `$/`
fallback that action methods rely on — pinned by `t/capture-var-topic-slot.t`).

The return-side env merges skip keys listed by
`is_routine_scoped_implicit_var`, which already covers `<name>`. That handles a
callee *writing* the slot. It does not handle a callee *removing* it: the
removal appears to reach the caller's base env directly rather than being
confined to the callee's overlay, so there is nothing left for the merge to
skip.

## Why it is not a one-liner

Making removal overlay-scoped is env-representation work (a tombstone in the
overlay that the merge can then discard), and the `t/capture-var-topic-slot.t`
constraint means the removal cannot simply become a `Nil` write. Both the
scoped-overlay path (`finish_light_env` and friends) and the
copy-and-restore paths would need to agree on the tombstone.

## Where it shows up

`t/match-vars-are-routine-scoped.t` documents the gap in a comment and asserts
only the baseline. No roast file in the current `MUTSU_REAL_TEST=1` residue
gates it — the three files that motivated the routine-scoping fix
(`S05-modifier/pos.t`, `S05-modifier/repetition-exhaustive.t`,
`S05-metachars/closure.t`) all use numeric captures.
