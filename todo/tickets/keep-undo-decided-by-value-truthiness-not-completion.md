# KEEP/UNDO are decided by the block's trailing-value truthiness, but real raku decides by normal-completion-vs-exception

Found while fixing `todo/tickets` (given/if `LEAVE`-phaser topic-clobber
regression, PR #6635) and writing its regression test
(`t/block-scope-trailing-value-does-not-clobber-topic.t`): the *existing*
local test `t/enter-phaser-rvalue.t` test 13 ("block ending in a falsy value
runs UNDO") does not actually match real `raku`.

## Repro

```raku
my $s = "";
{ KEEP { $s ~= "K" }; UNDO { $s ~= "U" }; 0 }
say $s;
$s = "";
{ KEEP { $s ~= "K" }; UNDO { $s ~= "U" }; False }
say $s;
```

```
raku:  K
       K
mutsu: K
       U     <- wrong: mutsu runs UNDO because the trailing value (False) is falsy
```

Confirmed with `try`/`die` that real `raku`'s actual rule is normal
completion vs. exception, not value truthiness:

```raku
my $s = "";
try { KEEP { $s ~= "K1 " }; UNDO { $s ~= "U1 " }; die "boom" }
say $s;   # raku: U1  (both raku and mutsu agree here)
```

So a block ending in `0`/`False`/any falsy-but-non-exceptional value still
runs **KEEP** in real Raku; only an actual thrown exception (or presumably
`fail`, not yet separately verified) runs **UNDO**. mutsu's
`should_run_success_queue` (`src/vm/vm_misc_scope.rs`, consulted by
`exec_block_scope_op`) currently treats a falsy `body_value` as "the block
failed" and routes to UNDO instead of KEEP, which is wrong.

## Where it goes

`src/vm/vm_misc_scope.rs`, `Self::should_run_success_queue(&body_result,
body_value)` — currently checks `body_value`'s truthiness (grep its
definition; not read in detail during this investigation). It should instead
only look at `body_result` (`Err` = exceptional exit -> UNDO, `Ok` = normal
completion -> KEEP), ignoring the value entirely.

## Existing test with a wrong assertion

`t/enter-phaser-rvalue.t` test 13 ("block ending in a falsy value runs
UNDO") asserts the current (wrong) mutsu behavior, not real raku's. Whoever
fixes this ticket should also fix that assertion (verified against real
raku: KEEP fires for a falsy-but-normal trailing value; only an exceptional
exit runs UNDO).

## Severity

Unclear how many other tests key off this rule; not yet checked whether
roast has a specific test for KEEP/UNDO truthiness vs. completion semantics.
Not blocking anything currently in progress — flagged for a future session.
