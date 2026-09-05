# A regex code block's `my` declaration survives the block's re-run

A `{ … }` block inside a regex runs again whenever the engine reaches it again.
mutsu lost the block's own `my` declarations from the second run onwards: the
variable read back as the enclosing scope's same-named binding, or as `Any` when
there was none.

```
raku  -e 'if "aaab" ~~ / (\w)+ { my $v = "Q"; my $p = $/.pos; say "v=", $v } b / {}'
v=Q
v=Q

mutsu -e '<same>'                       # before
v=Q
v=(Any)
```

## The ticket's diagnosis was wrong in two ways

It reported the trigger as the combination of `my` **and** reading the cursor
(`$/` / `$¢`), on the theory that materialising the lazy Match made the cursor
bindings look *rebound*. Neither half holds:

- The cursor is irrelevant. `my $p = "x".chars` reproduces it exactly; reading
  `$/` without calling a method on it (`my $p = $/`) does not. Any **method
  call** in the body is the trigger, because the clobber rides on the call.
- Backtracking is irrelevant. `"ab" ~~ / [ \w { … } ]+ /` — the same block run
  twice going *forwards* through a quantifier — fails identically.

The real rule is narrower and broader at once: **the second and later execution
of any regex code block loses every scalar `my` it declared before its first
call.** Ordering shows it cleanly — `my $p = "x".chars; my $v = "Q"` is fine (the
declaration comes after the call), and an assignment made after the call sticks.

## Root cause

`eval_regex_code_block_body` reports every env name the body **rebound** into
`pending_local_updates`, the log an embedded block uses to publish a write to an
*outer* lexical back into the caller's compiled local slots (`/ (\d) { $seen = $/.Str } \d+ /`
has to leave `$seen` set). The body's own `my` names were reported too. Nothing
filtered them out, so the VM treated a block-local name as a caller lexical —
and `drain_pending_local_updates_after_call` → `writeback_match_locals` refreshes
such a name **from `env`** at the body's next call. On the re-run, `env` no
longer holds the block-local (it was restored on the way out of the previous
run), so the freshly initialized slot was overwritten with the outer binding or
left as `Any`.

The same leftover log was also a live panic. `eval_regex_inline_code` brackets
the body with `let before = pending_local_updates.len()` … `split_off(before)`,
but the body's own call drains the list, so `before` could exceed the length:
`my $v = "OUT"; if "aaab" ~~ / (\w)+ { my $v = "Q"; my $p = "x".chars; … } b /`
aborted with `` `at` split index (is 2) should be <= len (is 1) ``.

## Fix

`eval_regex_inline_code` now collects the body's top-level `my` names (it already
walked the statements to build the `scoped` save/restore list) and filters them
out of `pending_local_updates`, alongside the two exclusions that were already
there — the `make` slot and the regex's own `:my`/`:let` lexicals, which are
lexical for exactly the same reason. The `split_off` index is clamped to the
current length, so an inner drain can no longer panic.

Writes to genuine outer lexicals are untouched: they are not `my`-declared in the
body, so they still travel through the log.

## Pin

`t/regex-code-block-my-lexical-rerun.t` (9 assertions), covering the backtracked
re-run, the plain-method-call trigger, the forward quantifier re-run, `make` of a
block-local, non-interference with a same-named outer lexical in both directions,
and that an outer write still reaches the caller on every run. Passes under real
`raku` as well as mutsu.
