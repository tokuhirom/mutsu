# Bare `??` in term position should raise `X::Syntax::DuplicatedPrefix`

rakudo diagnoses a doubled `?` the same way it does `^^`/`~~`:

```
$ raku -e 'say ??1'
Expected a term, but found either infix ?? or redundant prefix ?
  (to suppress this message, please use a space like ? ?)
```
(`X::Syntax::DuplicatedPrefix`, `prefixes => "??"`). mutsu currently gives a
different message via a different path: `my $x = ??1` raises
`X::Syntax::Malformed: Malformed initializer`, and `say ??1` raises
`X::Syntax::Confused`. No roast file currently pins this exact string — it is a
pure mutsu-vs-raku divergence found by doc-diff-style comparison, not a
whitelist blocker.

## The naive fix breaks a whitelisted roast file — do not repeat this

Adding `?` to `duplicated_prefix_run` (`src/parser/expr/postfix/loop_.rs`) with
the obvious guard — only a run of *exactly* two counts, since `???` is the
warn-flavoured yada stub and a real term — makes `say ??1` correct
(`X::Syntax::DuplicatedPrefix`) and does not regress `t/routine-yada.t`,
`t/hyper-postfix-dotted-wordy.t`, `t/parser-batch3.t`,
`t/stub-and-supersede.t`, `roast/S03-operators/misc.t`,
`roast/S03-operators/precedence.t`, or `roast/S02-types/WHICH.t`.

It DOES regress `roast/S03-operators/ternary.t` test 28 (`Z??`/`X??` must raise
`X::Syntax::CannotMeta`, not `X::Syntax::DuplicatedPrefix`) — but only for the
`Z`/`X` meta-prefixes; `R??`/`S??` keep passing. This was confirmed empirically
(2026-08-06): before the `?` change, `EVAL "1 Z?? 2 !! 3"` from a plain script
raises `X::Syntax::Confused` directly, yet the SAME string run through
`Test.rakumod`'s real `throws-like` (which calls `EVAL $code, context =>
$caller-context`, not a bare top-level `EVAL`) reports `X::Syntax::CannotMeta`.
The mechanism is `Test.rakumod`'s own fallback: its `CATCH` block checks
`$ex ~~ X::Comp::Group` and searches `.panic`/`.sorrows` for a match when the
top-level exception type doesn't match directly — so mutsu is apparently
already collecting a `CannotMeta` diagnosis as one of several candidate
"sorrows" while parsing `Z??`/`X??`, even though the *primary* reported error is
something else. Adding the naive `?` duplicated-prefix check makes
`DuplicatedPrefix` win in `prefix_expr` before whatever currently produces that
`CannotMeta` sorrow gets a chance to run, so the group no longer contains a
`CannotMeta` candidate and `throws-like`'s fallback search fails.

**Fix order, per the shape of `news/2026-08/metaop-doubled-infix-base.md`'s
`^^` fix:** find where the `Z??`/`X??` `CannotMeta` sorrow currently gets
generated (it is NOT one of the hardcoded `"X::Syntax::CannotMeta: Cannot do .
because..."` strings in `src/parser/expr/precedence/comparison.rs` /
`list_infix_loop.rs` — those are all for the `.` metaop, not `??`) before
touching `duplicated_prefix_run`. Confirm with a script that reproduces
`Test.rakumod`'s exact call shape (`EVAL $code, context => $ctx`, not a bare
`EVAL` in a fresh `try`) — a plain top-level `EVAL` does not exhibit the
CannotMeta sorrow at all, so it is not equivalent for testing this. Once `Z??`
and `X??` raise `CannotMeta` directly (not just as a buried sorrow), the `?`
addition to `duplicated_prefix_run` is safe to land alongside it. Verify with
the full `roast/S03-operators/ternary.t` and `misc.t`, not just `make test`.

Affected: `src/parser/expr/postfix/loop_.rs`
(`duplicated_prefix_run`/`prefix_expr`), and whatever currently produces the
`Z??`/`X??` `CannotMeta` sorrow (not yet located — likely somewhere in
`src/parser/expr/precedence_meta_ops/` or the multi-error/sorrows collection
in `src/parser/mod.rs`).
