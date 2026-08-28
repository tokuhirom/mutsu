# `$/` and the capture variables are scoped per routine, like `$!`

Three whitelisted roast files regressed under `MUTSU_REAL_TEST=1` for one
reason: a routine that performed a regex match internally overwrote its
**caller's** `$/`, `$0` and `$1`. In Raku those are implicitly `my`-declared in
every routine, exactly like `$!`, so the caller's values must survive the call.

## The repro has nothing to do with `Test`

```raku
sub inner() { "zz" ~~ /(z)/; 1 }
"abc" ~~ /(b)(c)/;
say ~$/, ' ', ~$0;    # bc b
inner();
say ~$/, ' ', ~$0;    # was `z z`, must stay `bc b`
```

## Why only the real `Test` provider saw it

mutsu's native `Test` is Rust and never runs Raku-level code between two
statements of a test file. The vendored `Test.rakumod` does — and on a
**failing** assertion it runs a good deal more of it, because rendering the
diagnostics (`# expected: …` / `# got: …`, and `diag`'s indentation, which
splits and re-joins on newlines) matches internally. So the pattern that broke
was always the same shape: an assertion fails, and the *next* statement in the
test file reads a `$/` that the failure's own diagnostics had clobbered.

```
ok 1 - matched
not ok 2 - first entry (deliberately failing)
Use of Nil in string context      <-- the next statement's $/[1] is gone
not ok 3 - second entry
```

That is why all three files had a `#?rakudo todo`-marked failing assertion
immediately before the assertion that actually regressed:
`S05-modifier/repetition-exhaustive.t` reads `$/[1]` after `$/[0]`,
`S05-modifier/pos.t` reads `$/.to` after an interpolation test that rakudo also
fails, and `S05-metachars/closure.t` reads `$0` after a `make`-inside-a-closure
test that is likewise TODO-marked.

## The fix

`runtime::utils::is_routine_scoped_error_var` — the predicate every return-side
env merge consults to decide which magic names must *not* be copied from callee
to caller — knew only about `$!`. It is now
`is_routine_scoped_implicit_var` and covers `$/` as well, plus the capture
variables that are views into it: mutsu stores `$0`, `$1`, … under the env keys
`0`, `1`, … and `$<name>` under `<name>`, so scoping `$/` alone left the caller
with the right `$/` and the wrong `$0`.

The predicate is applied only where the callee is a routine
(`cf.code.is_routine`). A bare block deliberately keeps sharing its enclosing
routine's `$/` and `$!` — `if $x ~~ /y/ { }` must leave `$/` visible after the
`if`, and a `CATCH` block writes `$!` in the routine that owns it — so extending
this to blocks would break `try`/`CATCH` and ordinary conditional matches. The
pin asserts both directions.

## Two neighbouring gaps split off rather than folded in

Both were verified to predate this change (by reverting the predicate to its old
body and rebuilding), and neither is gated by any roast file in the current
residue:

- A routine's named-capture reset *removes* the caller's `$<name>` slot rather
  than shadowing it, so the merge has nothing to skip —
  `todo/tickets/named-capture-reset-removes-the-callers-slot.md`.
- A block invoked through a `&`-parameter from inside another routine does not
  publish its match to the scope the block was written in —
  `todo/tickets/a-blocks-match-does-not-reach-its-defining-scope-through-a-callable.md`.

Pin: `t/match-vars-are-routine-scoped.t` (14 assertions, green under real `raku`
as well as mutsu).
