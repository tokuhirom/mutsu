# Retired the native `Test::Util` / `Test::Tap` overrides

`use Test::Util` and `use Test::Tap` load roast's real helper modules from
source (`roast/packages/Test-Helpers/lib/`), but mutsu's native TAP provider
still answered every name in `Interpreter::is_test_function_name` — `is_run`,
`doesn't-hang`, `is-path`, `is-eqv`, `warns-like`, `group-of`, `make-temp-file`,
… — before the imported routine was ever consulted. That is a rung-3 native
provider sitting on top of a module the interpreter can already parse, load and
run, so it goes the way `Pod::To::Text` and the native `Test::Tap` arm went.

`user_test_decl_beats_native` now consults the wide name set instead of the
`Test` module's own export list, so a *declaration* found in the loaded helper
module wins. The native handlers survive only as the fallback for a file that
calls a helper without loading its module.

## What actually blocked it

Only one roast file stood in the way at the end, and it was not a `Test::Util`
incompatibility: `S03-operators/repeat.t` test 56,

```raku
warns-like { 'x' x Int }, *.contains('uninitialized' & 'numeric'),
    'using an unitialized value in repeat count throws';
```

The real `warns-like` catches the warning with
`CONTROL { when CX::Warn { $did-warn = True; $message = .message; .resume } }`.
mutsu's `"x" x Int` wrote its warning straight to stderr — no CONTROL handler
ever saw it — so `$did-warn` stayed `False`.

Routing the raise through `raise_resumable_warning` (the mechanism
`news/2026-08/a-warning-resumes-at-its-raise-site.md` added for `Int.Numeric`)
was necessary but not sufficient: the handler ran, set `$did-warn = True`, and
the write was then discarded. The reason is a leaf-closure optimisation in
`call_compiled_closure_with_topic`. Its return path skips the caller-writeback
env scan when the closure has no free-variable change, no rw parameters, no
env-write opcode **and makes no calls at all** — the reasoning being that
without a call nothing outward can have been mutated. An inline CONTROL handler
is exactly the counterexample: `{ 'x' x Int }` makes no calls, the warning comes
out of an arithmetic opcode, and the handler writes the *installing* frame's
lexicals into the current frame's env with no call boundary to mark it.

`Interpreter::inline_control_env_writes` closes that hole. It is bumped every
time `try_resume_safe_control_inline` flushes a handler-mutated name to env;
each closure frame snapshots it on entry and forces the writeback scan when it
moved. `{ Int.Numeric }` never showed the bug only because a method call sets
`cc.has_calls`, which forced the scan anyway.

`todo/deep/inline-control-handler-from-a-non-call-op.md` recorded this as "the
handler body runs twice and neither of its writes reaches the frame". The double
run was a red herring: `raku` runs it twice too. The repro's own
`say "d=$d m=$m"` interpolates an *undefined* `$m`, which warns a second time —
so the second `HANDLER` is that warning's handler run, and it disappears in raku
only because the first handler run had already given `$m` a value. There was one
bug, not two.

## Measured

All 228 whitelisted roast files that `use Test::Util` pass
(`MUTSU_FUDGE=1 prove -j4 -e 'target/debug/mutsu' - < tmp/testutil-files.txt`).
On top of `MUTSU_REAL_TEST=1` this flip was previously measured as 343 → 315
regressions; it is now clean on its own terms.

One local test file had to be corrected rather than the interpreter:
`t/warns-like.t` asserted that `warns-like 'warn "first"; warn "second"'`
captures the *first* message. The real `warns-like` overwrites `$message` per
resumed warning, so it captures the last one — `raku` fails that assertion too.

Pins: `t/warn-from-a-non-call-op-reaches-control.t` (all six assertions pass
under `raku` as well), plus the 228 roast files.
