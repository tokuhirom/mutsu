# A placeholder at an EVAL'd unit's mainline is `X::Placeholder::Mainline`

`EVAL '$^a'` raised nothing at all, and `EVAL '@_'` raised `X::Undeclared`, where
rakudo raises `X::Placeholder::Mainline` for both. A placeholder parameter used
outside any sub or block is that error, and `@_` / `%_` are placeholders too —
which is why the check has to run *before* the undeclared-variable check, or the
`@_` case is reported as an undeclared variable instead.

## It was already implemented, in only one of two parallel check chains

`Interpreter::check_eval_mainline_placeholders` (`runtime/system_eval_vars.rs`)
has existed for a while, complete with the `placeholder` attribute and rakudo's
message text. Its only caller was `runtime/test_functions/throws_like.rs` — the
**native** `Test` provider's `throws-like`, which parses its code string itself
and runs its own chain of `check_eval_*` calls. The ordinary EVAL path
(`parse_and_eval_with_operators`) runs the same chain and was missing this one
member, so the check simply never fired for real `EVAL`.

That is why `roast/S32-exceptions/misc2.t` passed under the native provider and
failed under `MUTSU_REAL_TEST=1`: the real `Test.rakumod`'s `throws-like` EVALs
its string through the ordinary path.

The fix is one line in `parse_and_eval_with_operators`, placed ahead of
`check_eval_undeclared_vars` for the ordering reason above.

## The native call site is NOT redundant, and removing it was measured wrong

The obvious follow-up — "the native chain's copy is now a duplicate, delete it" —
was tried and **reverted**, because it is not a duplicate: the native
`throws-like` never goes through `parse_and_eval_with_operators` at all. With the
line removed, `roast/S32-exceptions/misc2.t` test 14 (`throws-like '@_',
X::Placeholder::Mainline`) fails under the *native* provider, since
`check_eval_undeclared_vars` then reports `X::Undeclared` first. Both chains
genuinely need the check; the call site now carries a comment saying so and
naming the test that proves it.

This is worth recording as a shape: a check reachable from only one of two
parallel chains looks exactly like a native-provider leniency crutch (the kind
`news/2026-08/parse-error-exception-classes.md` correctly retired), and is not
one. Measure the removal before believing the label.

## What it freed

`roast/S32-exceptions/misc2.t` passes under both providers — three assertions,
`throws-like '$^x'` / `'@_'` / `'"foo".{ say $^a }'`, all wanting
`X::Placeholder::Mainline`.

Pin: `t/eval-mainline-placeholder.t`, 15 assertions covering the mainline cases,
the `placeholder` attribute and message, the `X::Placeholder::Block` cases that
must stay distinct, the positions where a placeholder is legal, and an ordinary
undeclared variable that must still be `X::Undeclared` — green under real `raku`
unchanged.
