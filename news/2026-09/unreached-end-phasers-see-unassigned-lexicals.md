# An unreached `END` phaser sees unassigned lexicals, not the live ones

`news/2026-09/end-phasers-install-at-compile-time.md` made mutsu install every
`END` a compunit declares at compile time, so one inside a block that never runs
still runs at exit, as in rakudo. What such a phaser's body *sees* was still
wrong — and, once measured properly, wrong in a bigger way than the ticket
recorded.

## What rakudo actually does

An `END` is a closure. One that execution never reached was never **cloned**
against a live frame, so every `my`/`state` lexical it mentions reads as that
container's *unassigned* value — no matter which enclosing scope declared it,
and no matter what that scope later stored there:

```raku
my $t = 5;
my @arr = 1, 2, 3;
if False { my $x = 1; END { say $t.raku; say @arr.raku; say $x.raku } }
# Any
# []
# Any
```

The ticket only recorded the innermost case (`$x`), where mutsu answered `Nil`.
The outer ones were worse: mutsu answered the *live* `5` and `[1, 2, 3]`,
because a pre-installed phaser carried a deliberately empty env and every name
resolved against the exit-time env instead.

Everything that is **not** a per-frame lexical container still resolves
normally in rakudo — an uncalled `sub`, an `our` variable, a `constant`, a
class, `$*PROGRAM-NAME` — and mutsu keeps doing that.

## Fix

The compile-time walker (`EndWalker`, `src/runtime/end_phasers.rs`) now tracks
the lexicals visible at each descent point: each enclosing block's `my`/`state`
declarations that precede the point, plus routine, pointy-block and `for`-loop
parameters. `preinstall_end_phaser` seeds those names into the phaser's env at
their unassigned value (`Any` for a `$`, an empty `Array`/`Hash` for `@`/`%`)
and marks them in `dead_keys`, so the seed wins over a live same-named variable
further out — which is what shadowing requires (`my $w = 1; if False { my $w =
2; END { say $w } }` is `Any`). Reaching the declaration still replaces env and
`dead_keys` wholesale, so a captured phaser is untouched.

Names are seeded in the AST's own spelling — an `@`/`%` variable keeps its
sigil, a `$` variable is stored bare (`my $x` is `VarDecl { name: "x" }`), which
is also how the env keys them.

## Three deliberate limits

- **A top-level `END` is not seeded.** It is always reached, and it closes over
  the unit scope, which is still alive at exit — which is why
  `Interpreter::run` drops such a declaration from the compiled body outright
  rather than letting `PhaserEnd` capture it. A seed would therefore be the
  *only* binding it ever gets, and `my $hist; END { say $hist }; $hist ~= "x"`
  would print `Any` instead of the live value (caught by
  `roast/S04-phasers/multiple.t`). The walker tracks statement-list depth and
  seeds only at depth > 1.
- **Inside a `class`/`role`/`package` body, nothing is seeded.** The compiler
  package-qualifies a lexical's env key there
  (`Compiler::qualify_variable_name`) and the walker does not reproduce that
  mangling, so a bare-name seed would install a binding the body never reads.
  Such an `END` keeps the pre-seeding behaviour.
- **An uncalled routine's parameter reads as `Any`.** rakudo answers `VMNull`
  there — a raw NQP null whose `.defined` throws
  `X::Method::NotFound ... for invocant of type 'VMNull'`. That is an artifact
  of its binder rather than a Raku value, so mutsu answers `Any`, which agrees
  with rakudo on every defined question.

## Scope

`t/end-phaser-compile-time-install.t` grows eight rows (17 total): the `$`
lexical, an outer lexical, an assigned outer `Array`, the shadowing case, a
routine that ran but whose `END` was not reached, the non-lexical names that
must keep resolving, the top-level `END` invariant, and the parameter row
above. All were measured against
rakudo 2026.07 and all but the deliberately-divergent parameter row pass under
`raku` unchanged.

Found next door and filed rather than folded in:
`todo/tickets/an-our-declared-in-a-never-run-block-is-not-installed.md` — an
`our` in a dead branch is never installed at all, which is a package-symbol
question rather than a closure-lexical one.
