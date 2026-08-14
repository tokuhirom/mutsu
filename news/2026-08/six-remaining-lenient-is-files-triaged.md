# Test-vendoring sweep: lenient-`is` test files fixed, and the last six triaged individually

Found while re-running the Test-vendoring bulk sweep
(`todo/tickets/vendor-real-test-module.md`). Most of the sweep's failures were
**test-file** bugs, not interpreter bugs: rakudo's real `Test.rakumod` fails
them, and so does `raku` itself, because mutsu's native `is` stringifies its
arguments more eagerly than Raku's does.

## The shapes

**A type object compared against its gist spelling.** `is Point.WHAT, '(Point)'`
passes under mutsu's native provider and fails everywhere else, because Raku's
`is` compares `$got.Str` — and a type object's `.Str` is the empty string with a
warning, not its `.gist`:

```
$ raku -e 'use Test; plan 1; class Point {}; is Point.WHAT, "(Point)", "what"'
1..1
not ok 1 - what
# expected: '(Point)'
#      got: (Point)
```

`.gist` (or `.^name`, or `isa-ok`) is what these assertions actually mean.

**A lazy `Seq` compared against its reified contents.** `is $fh.lines, 'A B C'`
passes natively and gives `'(...)'` under the real module — again matching Raku,
which does not reify a lazy sequence to stringify it. `is $fh.lines.join(' '),
'A B C'` (or `is-deeply` against a list) is the assertion that survives.

**`Empty` compared against `Nil`.** `andthen` / `notandthen` yield `Empty` — an
empty `Slip` — when they skip their RHS, and so does a routine whose body ends in
a statement-modifier `if` that does not fire. `is $x, Nil` passes natively and
fails under the real module *and* under `raku`; `is-deeply $x, Empty` is the
assertion that holds.

## Corrected (2026-08-01)

`news/2026-08/test-files-asserted-against-a-lenient-is.md` — 19 files, 40
assertions, each verified three ways (mutsu's native provider, the aliased
upstream `Test.rakumod`, and `raku`):

- 35 `is <expr>.WHAT, '(Type)'` assertions across 15 files became
  `is <expr>.WHAT.gist, ...`.
- `t/lock.t` wanted a *qualified* name, which `.gist` does not give (`.gist` of
  `Lock::Async` is `(Async)` in raku too), so it asks `.^name` instead.
- 4 `is …, Nil` assertions across 4 files became `is-deeply …, Empty`. One of
  them exposed a real compiler bug — `notandthen` loaded `Nil` instead of the
  empty `Slip` its `andthen` sibling loads — which is fixed in the same change.

## A third shape: `lives-ok` takes a `Callable`

`t/variable-traits.t` passed a `Str` to `lives-ok`. The string form is
`eval-lives-ok`; raku rejects the call at compile time (*Calling lives-ok(Str,
Str) will never work*), and mutsu's native provider accepted it. It was the only
such call in the whole of `t/` — corrected in
`news/2026-08/pod-begin-at-end-of-input.md`, which also fixed
`t/pod-begin-without-identifier.t` for asserting that a mid-line `=begin` is a
Pod directive (raku reads it as an infix `=` in term position; a Pod directive
has to start a line).

## The last six, triaged individually (2026-08-14)

The remaining six files from the sweep's "raku fails it too" bucket were **not**
this problem — in each of them `raku` fails for a reason unrelated to `is`'s
leniency, so the `raku` verdict said nothing about assertion style and each
had to be read on its own. All six are now resolved, four of them exposing
genuine general interpreter bugs (not test-file issues):

- **`begin-phaser-begintime.t`** — real bug: `check_phaser_depth` (the counter
  `CheckPhaserStart`/`CheckPhaserEnd` use to know whether an error should be
  wrapped in `X::Comp::BeginTime`) leaked past a BEGIN/CHECK body that errored,
  because the early-return error path in `run_inner`
  (`src/vm/vm_run_loop.rs`) skipped the matching `CheckPhaserEnd` decrement.
  A *later*, unrelated error on the same `Interpreter` (an `INIT` phaser's
  plain `die`) then inherited the stale depth and was wrongly wrapped too.
  Fixed by snapshotting the entry depth and restoring it on every error exit.
  Also documented a narrow, low-value Rakudo quirk this surfaced —
  `todo/tickets/begin-rat-divzero-escapes-wrapping.md` (a Rat div-by-zero
  triggered lazily inside `BEGIN` escapes `raku`'s own wrapping, unlike every
  other exception shape) — left unmatched rather than chased.
- **`listop-arg-loose-logical-precedence.t`** — already fixed by earlier,
  unrelated work; passes cleanly under both providers with no changes needed
  this round.
- **`method-private-errors.t`** — test-file bug: its `trusts Caller` probe
  referenced class `Caller` before declaring it, which `raku` itself rejects
  (forward references need a stub, `class Caller {...}`) — mutsu happened to
  be lenient about the order. Added the stub so the test is valid under real
  `raku` too, not just under mutsu's more permissive resolution.
- **`placeholder-named-in-method-do.t`** — two issues: (1) test-file bug, the
  mainline-`do{}` assertion expected `X::Placeholder` (a bare package in
  `raku`, not an instantiable exception class) instead of the actual
  `X::Placeholder::Block`; (2) real bug, `compile_do_block_expr`
  (`src/compiler/helpers_do_expr.rs`) exempted *both* `%_` and `@_` from the
  "unattached placeholder in a signature-less block" check inside a method,
  but `raku` only auto-adds `*%_` to a method, never `*@_` — a bare `@_`
  anywhere in a method body is a compile error there. Fixed to exempt only
  `%_`; the test's `@_`-in-`do{}` case was rewritten from a false-positive
  "resolves positional args" assertion into a `throws-like` matching real
  `raku`. The parallel direct-usage (no `do{}`) leniency is a separate,
  slightly larger fix, tracked in
  `todo/tickets/method-direct-at-underscore-should-be-rejected.md`.
- **`use-version-short-adverb.t`** — real bug: `use Foo:v<1.2.3>` (the short
  spelling of `:ver<1.2.3>`) was parsed as an unknown import tag instead of
  the version selector. Fixed in `src/parser/stmt/decl/use_decl.rs` by
  accepting `v` as a peer of `ver`/`auth`/`api` and canonicalizing it to
  `ver`.
- **`vm-panic-boundary.t`** — real bug, partially fixed: a Rust panic caught
  at a `catch_unwind` boundary (`try`/`CATCH`, `EVAL`) left every call frame
  pushed since the boundary un-popped, because the callee's own
  `pop_call_frame()` (which restores `locals`/`upvalues`/`env`) never runs on
  an unwind. The next `GetLocal` after recovery then indexed with a slot
  number valid for its own frame, not the leftover callee one — an immediate
  secondary crash. Fixed generally via
  `Interpreter::recover_call_frames_after_panic`, wired into both
  `catch_unwind` sites; regression test `t/panic-recovery-call-frames.t`. A
  second, deeper layer remains: `current_package` and pragma state are saved
  outside the `call_frames` mechanism (as plain Rust locals in
  `call_compiled_closure_with_topic`/`call_compiled_function_named_inner`) and
  leak the same way, which is why this file still fails one subtest later
  under `MUTSU_REAL_TEST=1` specifically (`unqualified `proclaim` not found
  after a panic recovers mid-`dies-ok`). Tracked as its own architectural
  slice: `todo/deep/panic-unwind-leaks-side-channel-call-state.md`. Under
  mutsu's native `Test` provider (what `make test`/CI actually run) the file
  is unaffected either way — 9/9 before and after.

`t/variable-traits.t` was out of this bucket and is tracked separately:
`todo/tickets/user-trait-mod-multi-shadows-builtin-traits.md` (still open,
unrelated to this finding).

With this round, every file `todo/tickets/vendor-real-test-module.md`'s bulk
sweep flagged for `t/` has been triaged to a conclusion — fixed, corrected, or
handed off to its own tracked ticket.
