# `schedule-on` no longer loses the whenever body's lexical env

When a supply was tapped through `.schedule-on($*SCHEDULER)`, the `whenever`
body used to execute WITHOUT the supply block's lexicals in env: a `my enum`
declared in the supply block was not visible, so a bareword enum value died
with `X::Undeclared::Symbols: Undeclared name` (raised from
`exec_get_bare_word_op`, on the main VM thread). The same code without
`.schedule-on` worked.

Repro (needs a module that makes `Header` a suppressed name, e.g.
`t/suppressed-type-vs-local-decl-lib/SuppMod.rakumod` — without a
suppressed-name collision the bareword still resolved via the registry, so
the env loss was masked):

```raku
use SuppMod;
my $in = Supplier.new;
my $out = supply {
  my enum E <A Header B>;
  whenever $in -> $v { emit Header.Int }
};
$out.schedule-on($*SCHEDULER).tap: -> $x { say "got $x" },
    quit => -> $ex { say "QUIT: {$ex.gist}" };
$in.emit(1);
sleep 1;
```

used to print `QUIT: X::Undeclared::Symbols: ... Header used at line 1`
instead of `got 1`.

This blocked `Cro::HTTP::ResponseParser`'s `transformer(...)` helper, which
taps through `.schedule-on($*SCHEDULER)` and declares `my enum Expecting
<StatusLine Header Body>` in its supply block.

## Resolution

Re-checking this ticket's own repro on current `main` (2026-08-07) showed it
already passes — no `schedule-on`-specific fix was ever landed for it
directly. `git bisect` against the repro (`tmp/schedule-on-repro.raku`
matching the ticket text) pinned the fix to
[#5826](https://github.com/tokuhirom/mutsu/pull/5826), "END phasers run in
reverse install order, not registration order"
(`news/2026-08/end-phasers-run-in-install-order.md`): that PR's broader
module/env lifecycle work (`src/runtime/run.rs`, `run_modules.rs`,
`runtime_thread.rs`) fixed the whenever-body env-capture loss as a side
effect, even though its own title and description are about `END` phaser
ordering.

Added `t/schedule-on-whenever-env.t` to pin the fix going forward, using
`CurrentThreadScheduler` (synchronous, deterministic) instead of
`$*SCHEDULER` + `sleep` so the test doesn't depend on timing.

## Related

The non-`schedule-on` path (a suppressed module-lexical type not clobbering
a same-named local declaration) was fixed earlier by
[#5628](https://github.com/tokuhirom/mutsu/pull/5628); see
`t/suppressed-type-vs-local-decl.t`.
