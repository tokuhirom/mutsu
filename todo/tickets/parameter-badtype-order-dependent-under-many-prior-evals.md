# `X::Parameter::BadType` (package-as-type) stops firing after ~47 prior `throws-like` EVALs in the same file

## Repro

Found while working `todo/tickets/vendor-real-test-module.md`'s
`roast/S32-exceptions/misc.t` gap list. The isolated two-line case works
fine:

```
$ MUTSU_REAL_TEST=1 target/debug/mutsu -I modules/Rakudo-Core/lib -e '
use Test;
plan 2;
throws-like q[my package A {}; my A $a;], X::Syntax::Variable::BadType;
throws-like q[my package A {}; sub foo(A $a) { }], X::Parameter::BadType;
'
# both pass
```

But the SAME two lines, run as `roast/S32-exceptions/misc.t` lines 226-227
after the file's preceding ~47 `throws-like`/other subtests have already
run, fail — the second `throws-like` no longer dies at all (its code, `my
package A {}; sub foo(A $a) { }`, silently succeeds where it should raise
`X::Parameter::BadType`):

```
$ head -229 roast/S32-exceptions/misc.t > /tmp/t.t   # (run from roast/S32-exceptions/ so
                                                       #  $*PROGRAM.parent(2) resolves)
$ echo 'done-testing;' >> /tmp/t.t
$ MUTSU_REAL_TEST=1 MUTSU_FUDGE=1 target/debug/mutsu -I modules/Rakudo-Core/lib /tmp/t.t
...
    not ok 1 - 'my package A {}; sub foo(A $a) { }' died
```

Bisected the *minimum trigger* by prepending truncated prefixes of the real
file: appending the test's own two lines after `head -226` of the real file
passes; appending them after `head -227` (i.e. running the file's own real
line 227, `throws-like 'my package A {}; my A $a;',
X::Syntax::Variable::BadType;`, for real before the target test) fails. So
the trigger needs the REAL preceding ~226 lines of file state — not
reproducible from the two lines alone, even including that exact preceding
`throws-like` call, tried standalone above and it still passed.

## Hypothesis (not confirmed)

`misc.t` reuses generic single-letter names (`A`, `foo`, etc.) across many
of its 182 independent `throws-like`-EVAL'd snippets. Working guess: some
global counter or cache used to disambiguate/uniquify a lexically-scoped
`package`/`class` declared inside an `EVAL` (needed so `A` in EVAL #40
doesn't collide with `A` in EVAL #5) wraps around, saturates, or otherwise
produces a colliding key after enough EVALs have run in the same process,
so by EVAL #47ish a *fresh* `package A {}` gets silently treated as
already-known (skipping the type-check pre-pass that raises
`X::Parameter::BadType`) instead of being freshly registered. Not
investigated further — the accumulated-state repro is expensive to run
(the full preceding subtest sequence) and no smaller trigger was found in
the time available this round.

## Where this was found

`todo/tickets/vendor-real-test-module.md`'s ongoing `roast/S32-exceptions/misc.t`
gap-closing (this round also fixed the `X::Inheritance::SelfInherit`
missing `.name` attribute, the EVAL-compiled `proto sub` being invisible to
`check_eval_undeclared_routines`'s `X::TypeCheck::Argument` path, and
`X::Parameter::BadType` itself missing from `register_x` — see that
ticket's latest round entry). This is the one remaining gap in that file
that looked deep enough to file separately rather than keep digging in the
same session.

## Suggested next step

Reproduce with a synthetic loop (`for ^60 { EVAL('package A {}; ...') }`,
varying what runs between iterations) to find the actual saturation
trigger without needing the full roast file, then trace whatever counter/
cache is implicated with `rust-gdb` breakpoints per CLAUDE.md's debugging
guidance.
