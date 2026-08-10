# Sinking a `try` block's discarded value throws *outside* the `try`

`roast/integration/advent2009-day20.t` aborts after 11 of its 21 assertions under
`MUTSU_REAL_TEST=1`:

```
# You planned 21 tests, but ran 11
Stub code executed
  in sub eval_exception at .../Test.rakumod line 1
  in sub eval-lives-ok at .../Test.rakumod line 593
```

The assertion is `eval-lives-ok 'map -> $x, $y { ... }, 1..6'` (line 28), and
`Test.rakumod`'s helper is

```raku
sub eval_exception($code) {
    try {
        EVAL ($code);
    }
    $!;
}
```

`raku` runs the same file and the same module and passes. mutsu does not merely
report a failed assertion — the exception escapes `eval_exception` entirely and
kills the file, which is why it costs ten assertions.

## What was measured

Every row below is `raku` vs mutsu on the same one-liner (all under
`use MONKEY-SEE-NO-EVAL`). "throws" means an *uncaught* `Stub code executed`.

| snippet | raku | mutsu |
| --- | --- | --- |
| `sub ee($c) { try { EVAL ($c); }; $! }` on `map -> $x,$y { ... }, 1..6` | no throw, `$!` undefined | **throws** |
| `sub ee() { my $r = try { EVAL (…); }; $! }` (value captured) | no throw | no throw |
| `sub ee() { try { map -> $x,$y { ... }, 1..6; }; $! }` (no EVAL) | throws | throws |
| `try { EVAL (…); }; say "made it"` at unit scope | throws | throws |
| `my $s = EVAL (…); $s.sink` | throws | — |
| `my $s = map -> $x,$y { ... }, 1..6; $s.elems` | throws | — |

So the divergence is exactly one cell: a `try` block whose **last statement is a
call** and whose value is discarded. `raku` does not sink that value at all;
mutsu sinks it, the sink reifies the `Seq`, the stub block runs and throws — and
the throw is reported *outside* the `try`, at the enclosing routine or unit.

Two independent things are wrong, and they should be judged separately:

1. **The sink happens outside the `try`'s protection.** Whatever the correct
   sink point is, an exception raised while discarding a `try` block's own value
   is currently uncatchable by that `try`. Note this alone is not enough to fix
   the roast file: it would turn the abort into a *failed* `eval-lives-ok`
   instead of a passing one.
2. **The value should not be sunk here at all.** That is what makes `raku` pass.
   Note the third row: `raku` *does* sink and *does* throw when the try block's
   last statement is the `map` itself, so the rule is not "a `try` block's value
   is never sunk". Rakudo's sink-context propagation is static, and a call's
   runtime result is evidently not covered by it the way a `map` statement is.
   Pin down that rule before changing mutsu's, or the third row regresses.

## Where to look

mutsu's statement-level sink for a block-valued statement is emitted in
`src/compiler/stmt.rs` (`SinkPop` and friends); the `try` compilation is
`compile_try` in the same file (see
`news/2026-08/block-local-routine-scope.md` for the `TryCatch { traps }`
distinction between a real `try` and the implicit one). The reify-on-sink of a
`Seq` is `force_lazy_list_vm` (`src/vm/vm_helpers_lazy.rs`).

Related: `todo/deep/deferred-seq-materialization-destroys-the-original.md` is the
other place in this campaign where the real module's strictness meets mutsu's
eagerness, but it is a different mechanism (a `.defined` probe, not a sink).

## Deep-dive investigation (2026-08-10)

**Headline result: the motivating symptom is already fixed on current main.**
`roast/integration/advent2009-day20.t` passes 21/21 both plain and under
`MUTSU_REAL_TEST=1` (verified 2026-08-10, debug build). The fix was PR #6115
(merged 2026-08-09, `e16d93a41`, branch `fix/try-lazy-seq-sink-in-scope`):
commit `a2808479c` gave the `...` stub `fail()` semantics instead of `die()`
(`src/runtime/builtins_control_flow.rs`, pinned by `t/stub-fail-semantics.t`,
news: `news/2026-08/stub-fail-semantics.md`), and `604a8bbb6` made unhandled
Failures throw in string-coerce context. The file is whitelisted
(`roast-whitelist.txt:1342`). The ticket's "mutsu" column above is therefore
stale for row 1 (mutsu no longer throws there). Rakudo's sink rule — the open
question this ticket asked to pin down — is now derived empirically below.

### Probe matrix (raku 2025.x vs mutsu current main, 2026-08-10)

Probe scripts: `tmp/sink-probes.sh`, `tmp/sink-probes2.sh`, `tmp/sink-probes3.sh`
(run as `sh tmp/sink-probes.sh raku` / `sh tmp/sink-probes.sh target/debug/mutsu`;
each mutsu invocation wrapped in `timeout 30`). "throws" = uncaught, nonzero exit.
Matches are marked =, divergences marked with the divergent side's output.

Group A — try in statement position, tail statement varies (unit scope):

| # | snippet | raku | mutsu |
| --- | --- | --- | --- |
| P1 | `try { fail "x" }; say "alive ", $!.defined` | `alive True` | = |
| P2 | `sub f { fail "x" }; try { f() }; say ...` | `alive True` | = |
| P3 | `my $v = try { fail "x" }; say $v.^name` | `alive Any` | = |
| P4 | `try { (1..3).map({die "boom"}) }; say ...` | **throws** | `alive True` (caught) |
| P5 | `sub f { (1..3).map({die "boom"}) }; try { f() }; say ...` | **throws** | `alive True` (caught) |
| P6 | `sub f { (1..3).map({ say "forced $_"; $_ }) }; try { f() }; say "done"` | forced 1..3, done | = |
| P7 | same with literal `.map` tail in try | forced 1..3, done | = |

Group B — plain statements without try (baseline; all match):

| # | snippet | raku | mutsu |
| --- | --- | --- | --- |
| P8 | `f(); say "done"` (side-effect Seq) | forced 1..3, done | = |
| P9 | `f(); say "alive"` (die Seq) | throws | = |
| P10 | `(1..3).map({ say "forced $_" }); say "done"` | forced, done | = |
| P11 | `sub f { fail "x" }; f(); say "alive"` | throws | = |
| Q13 | `my $s = (1..3).map({ fail "x" }); say $s.eager.raku` | throws | = |
| Q15 | `sub f { (1..3).map({ fail "x" }) }; f(); say "alive"` | throws | = |

Group C — try inside a sub, non-final statement (the ticket shape):

| # | snippet | raku | mutsu |
| --- | --- | --- | --- |
| P12 | `sub f { (1..3).map({die "boom"}) }; sub ee { try { f() }; $! }; say ee().^name` | **throws** | `X::AdHoc` + alive (caught) |
| P13 | same with literal `.map` tail | **throws** (`in sub ee`) | `X::AdHoc` + alive (caught) |
| P14/P15 | side-effect variants of P12/P13 | forced 1..3, done | = |
| P18 | `sub ee { try { f() } }; say ee().^name` (die Seq, try final, value used) | `Seq` + alive (never forced) | `Nil` + alive (forced+caught) |
| Q5 | `sub ee { try { map -> $x,$y { ... }, 1..6; }; $! }` (yada literal) | **throws** (`in sub ee`) | `Failure` + alive |
| Q6 | same via `sub f { map -> ... }` call | **throws** | `Failure` + alive |
| Q11 | `try { EVAL $c }` in sub, `$c` = die-Seq code | **throws** | `X::AdHoc` + alive |
| Q14 | `sub f { (1..3).map({ fail "x" }) }; try { f() }; say ...` (unit) | **throws** | `alive True` |

Group D — the EVAL / eval-lives-ok cell (all key cells MATCH now):

| # | snippet | raku | mutsu |
| --- | --- | --- | --- |
| P16/R4 | `sub ee($c) { try { EVAL ($c); }; $! }; ee(q[map -> $x, $y { ... }, 1..6])` | returns Failure, `.defined` False, alive | = |
| R9 | `not defined ee(...)` (the eval-lives-ok check) | `True` | = |
| R5 | R4 + `say "reached-tail"` before `$!` | "reached-tail" NOT printed — the yada `fail` fired during the post-try sink **early-returns the Failure from `ee`** | = (also no reached-tail) |
| R8 | R5 with block `{ say "invoked"; ... }` | invoked, no reached-tail, r=Failure | invoked, **reached-tail printed**, r=X::StubCode |
| P17/Q10 | `try { EVAL ... }; say "made it"` at **unit** scope (literal or runtime string) | **throws** | = (throws) |
| Q2 | `my $r = EVAL $c; say $r.^name` (value used) | `Seq` (lazy, unforced) | = |
| Q3 | ... `$r.eager` | throws Stub code executed | = |
| Q12 | `sub f { map -> $x,$y { ... }, 1..6 }; try { f() }; say ...` at unit | **throws** | = (throws) |
| Q4 | `try { map -> $x,$y { ... }, 1..6; }; say ...` at unit | **throws** | = (throws) |
| R6 | Q5 + reached-tail marker | throws (no marker) | `r=Failure` + alive (no marker — early return) |
| R7 | Q6 + markers | invoked, then **throws** | invoked, reached-tail, `r=X::StubCode`, alive |

Group E — where the sink happens relative to the protection:

| # | snippet | raku | mutsu |
| --- | --- | --- | --- |
| P22 | `try { (1..3).map({ die "boom" }); CATCH { default { say "caught-inside" } } }` | caught-inside, alive | = |
| P23 | same with tail call `f()` | caught-inside, alive | = |
| Q8 | P23 shape inside a sub | caught, alive | = |
| Q9 | `try { (1..3).map({die "boom"}) }; CATCH { default { say "unit-caught" } };` at unit | **unit-caught** (the escape is caught by the *enclosing* block's CATCH) | alive, no unit-caught (inner try already caught it) |
| P20/P21 | bare block tail/non-tail call sink | forced, done (+ useless-use warn P21) | = |

Group F — the underlying laziness split (root cause of every divergent cell):

| # | snippet | raku | mutsu |
| --- | --- | --- | --- |
| M2 | `sub f { (1..3).map({ say "invoked"; $_ }) }; my $x = f(); say "got"` | got first (lazy) | **invoked 1..3 first** (eager at call boundary) |
| M4 | `my $x = (1..3).map({ say "invoked"; $_ }); say "got"` | lazy | eager |
| M5 | `my $x = map -> $a,$b { say "invoked"; $a }, 1..6; say "got"` | lazy | eager |
| Q2 | EVAL-returned Seq | lazy | lazy (=) — EVAL is the one boundary mutsu does NOT reify through |

### The derived rule (the question this ticket left open)

Rakudo's sink-context propagation, stated from the data: **a statement-position
`try { ... }`'s value IS sunk — there is no "call results are exempt" rule** (the
ticket's hypothesis 2 is refuted: P6/P14/P16b force and print in raku). Sink
context propagates statically into immediately-invoked blocks onto their final
statement, but a plain `try`'s own handler wrapper interposes and stops that
propagation, so the tail value escapes the handler un-sunk and is sunk by the
generic statement-level sink at the `try` statement itself — **outside the
protection** (the ticket's hypothesis 1, "sink outside the try is a mutsu bug",
is also refuted: raku behaves the same way — P4/P5/P12/P13/Q4-Q6/Q12 all throw
uncaught in raku, and Q9 shows the escape is catchable by the *enclosing*
block's CATCH). When the block carries an **explicit CATCH phaser**, there is no
wrapper, sink propagates into the block, and force-time exceptions are raised
inside the protected region (P22/P23/Q8). Orthogonally, `try` absorbs a trailing
Failure *value* inside its protection (`$!` = `.exception`, result Nil —
P1/P2/Q1). What makes the roast cell pass under raku is a third mechanism
entirely: the yada `...` in the mapped block runs `fail`, and a `fail` fired
while the Seq is being forced at the post-try statement sink unwinds as a
control return to a live enclosing routine when the block's lexical chain (via
EVAL's caller-context compilation) resolves to one — `eval_exception`
early-returns an unhandled Failure (R5/R8: `reached-tail` never prints), whose
`.defined` is False, so `eval-lives-ok` passes. Without the EVAL boundary
(R6/R7) rakudo throws instead. Compile-time vs runtime split: the sink *marking*
is static (non-final statements; final statements of sunk blocks unless a
handler wrapper interposes); the *forcing* is runtime, performed by whichever
frame owns the marked statement.

### Mutsu's current state against that rule

- **SinkPop placement is already rakudo-conformant.** `Stmt::Expr(Expr::Try)`
  compiles the try (`src/compiler/stmt.rs:717-730`, `SinkPop` emitted at line
  729) *after* `compile_try` → `compile_try_region`
  (`src/compiler/helpers_control_flow.rs:564-566, 577-712`) has closed the
  `TryCatch` region (opcode emitted at line 621, tail value kept on stack at
  lines 646-655, `ThrowIfFailure` at line 670, region patched closed at lines
  705-710). So the statement sink (`OpCode::SinkPop` handler at
  `src/vm/vm_exec_dispatch.rs:2741`, LazyList forcing via `force_lazy_list_vm`
  at lines 2834-2836, defined in `src/vm/vm_helpers_lazy.rs`) runs outside the
  trap — exactly where raku runs it. **Do not move it inside.**
- **The EVAL cell matches** (P16/R4/R5/R9, and the roast file passes) because
  #6115's fail-semantics `...` + mutsu's fail-signal conversion at the sub
  boundary reproduce raku's early-return-of-Failure observable.
- **Every remaining divergent cell (P4, P5, P12, P13, P18, Q5, Q6, Q9, Q11,
  Q14, R6, R7, R8) traces to eager Seq reification, not to try/sink placement**:
  mutsu forces map Seqs at call/assignment boundaries (group F), so the
  force-time error surfaces *inside* the try (where mutsu's trap catches it)
  instead of at the caller's statement sink (where raku lets it escape). Mutsu
  is uniformly *more forgiving* than raku in these cells — it cannot abort a
  file raku would pass, only pass constructs raku would abort. These cells
  belong to the eagerness campaign
  (`todo/deep/deferred-seq-materialization-destroys-the-original.md`), not here.

### Implementation plan (for the implementing agent)

The semantics question is resolved and the motivating roast failure is fixed.
What remains is to **pin the now-correct behavior** so neither the sink
placement nor the `...`-fail mechanism regresses silently, and to hand the
residual cells to the campaign that owns them. Concretely:

1. Create branch `test/pin-try-statement-sink-semantics` off `main`.
2. Add `t/try-sink-semantics.t` with exactly the content below. It was verified
   2026-08-10 to pass 14/14 under BOTH `target/debug/mutsu` and `raku` (a true
   parity pin; a working copy exists at `tmp/try-sink-semantics.t`):

```raku
use MONKEY-SEE-NO-EVAL;
use Test;
plan 14;

# A. try absorbs a trailing Failure value inside its protection (raku: P1/P2/Q1)
try { fail "x" };
ok $!.defined, 'try absorbs a literal trailing fail, $! set';
sub ff() { fail "x" }
try { ff() };
ok $!.defined, 'try absorbs a call-returned trailing Failure, $! set';
sub q1() { fail "x" }
my $r = try { q1() };
ok !$r.defined, 'absorbed-Failure try returns an undefined value';
is $!.^name, 'X::AdHoc', 'try sets $! to the exception behind the Failure';

# B. statement-position try's value IS sunk (side effects run) (raku: P6/P7)
my @forced;
sub seq-se() { (1..3).map({ @forced.push($_); $_ }) }
try { seq-se() };
is @forced.elems, 3, 'statement-position try forces a call-returned Seq';
@forced = ();
try { (1..3).map({ @forced.push($_); $_ }) };
is @forced.elems, 3, 'statement-position try forces a literal map Seq';

# C. explicit CATCH sees the force-time die (raku: P22/P23/Q8)
my $caught = False;
try { (1..3).map({ die "boom" }); CATCH { default { $caught = True } } };
ok $caught, 'explicit CATCH catches a force-time die of the tail map';
$caught = False;
sub die-seq() { (1..3).map({ die "boom" }) }
try { die-seq(); CATCH { default { $caught = True } } };
ok $caught, 'explicit CATCH catches a force-time die of a call-returned Seq';

# D. the eval_exception / eval-lives-ok cell (raku: P16/R4/R9) — this is the
#    exact Test.rakumod shape that costs advent2009-day20.t when broken.
sub eval_exception($code) {
    try { EVAL ($code); }
    $!
}
my $e = eval_exception(q[map -> $x, $y { ... }, 1..6]);
ok (not defined $e), 'eval_exception of a lazy stub map is not defined';
ok $e ~~ Failure, 'eval_exception of a lazy stub map returns a Failure';

# E. unit-scope escapes: sinking a lazy stub map at unit scope kills the
#    program even under try (raku parity: Q4/Q10/Q12/P17) — subprocess checks
my $p = run($*EXECUTABLE, '-e', 'use MONKEY-SEE-NO-EVAL; my $c = q[map -> $x, $y { ... }, 1..6]; try { EVAL $c; }; say "made it"', :out, :err);
ok $p.exitcode != 0, 'unit-scope try{EVAL lazy stub map} still dies at the statement sink (raku parity)';
unlike $p.out.slurp(:close), /'made it'/, '... and does not reach the next statement';
$p = run($*EXECUTABLE, '-e', 'sub f { map -> $x, $y { ... }, 1..6 }; try { f() }; say "made it"', :out, :err);
ok $p.exitcode != 0, 'unit-scope try{call returning lazy stub map} dies (raku parity)';
unlike $p.out.slurp(:close), /'made it'/, '... and does not reach the next statement';
```

3. Verify locally (no src/ changes are needed or wanted):
   - `cargo build`
   - `timeout 60 target/debug/mutsu t/try-sink-semantics.t` → 14/14
   - `MUTSU_FUDGE=1 prove -e 'target/debug/mutsu' roast/integration/advent2009-day20.t` → PASS
   - `MUTSU_REAL_TEST=1 MUTSU_FUDGE=1 prove -e 'target/debug/mutsu' roast/integration/advent2009-day20.t` → PASS
   - `make test` (debug binary, per ADR-0014)
4. Append a short "residual try-cell divergences (2026-08-10)" section to
   `todo/deep/deferred-seq-materialization-destroys-the-original.md` listing the
   divergent shapes from the matrix above (P4, P5, P12, P13, P18, Q5, Q6, Q9,
   Q11, Q14, R6, R7, R8 — one-line snippets + raku-vs-mutsu outcome), stating
   that they are eagerness artifacts and will align automatically once
   sub-returned/assigned Seqs stay lazy, and warning that landing laziness makes
   mutsu STRICTER in those cells (a full-roast CI sweep is mandatory then).
5. `git mv` THIS ticket to `news/2026-08/try-statement-sink-semantics-pinned.md`
   and rewrite it as an accomplishment (headline: fixed by #6115's `...`-fail
   semantics; sink placement verified rakudo-conformant; parity pinned by
   `t/try-sink-semantics.t`; residual cells handed to the deferred-seq deep
   ticket). Per `todo/README.md`, `todo/` holds only open findings.
6. PR per CLAUDE.md workflow: conventional title
   `test(sink): pin try-statement sink semantics (raku parity matrix)`,
   `gh pr merge --auto --merge`, verify mergeStateStatus, background CI watch.

**Do NOT, under any circumstances:**
- Move the `SinkPop` for a try statement inside the `TryCatch` region, or make
  `TryCatch` trap SinkPop-time exceptions. That *sounds* like the ticket's
  original ask, but the matrix proves raku sinks outside the protection; moving
  it inside would break raku parity on Q4/Q10/Q12/P17 (unit-scope escapes, all
  currently matching) and re-break the semantics #6115 fixed.
- "Fix" the P4/P5/P12-type cells by special-casing `try` or by forcing the tail
  value inside `compile_try_region`. They are eagerness artifacts; the fix is
  Seq laziness (deep ticket), not try surgery.
- Revert or weaken `...`'s fail semantics (`t/stub-fail-semantics.t` pins it).

### Test plan

- New: `t/try-sink-semantics.t` (above, 14 assertions, raku-verified).
- Existing pins that must stay green (the sink behavior is load-bearing):
  `t/stub-fail-semantics.t`, `t/statement-call-sinks-its-value.t`,
  `t/sink-tail-failure.t`, `t/sink-method-context.t`, `t/sink-warning.t`,
  `t/dot-eq-sink.t`, `t/failure-sink-handled.t`,
  `t/imported-sub-shadows-builtin-in-sink-position.t`,
  `t/throws-like-gather-sink.t`, `t/regex-sep-quantifier-ratchet.t` (adjacent).
- Roast: `roast/integration/advent2009-day20.t` stays 21/21 in both plain and
  `MUTSU_REAL_TEST=1` modes; it is whitelisted (`roast-whitelist.txt:1342`) and
  per CLAUDE.md a whitelisted file must never regress — CI's full `make roast`
  is the safety net for the rest of the suite.

### Regression hazards

- The pinning test's group E spawns subprocesses via `$*EXECUTABLE`; keep it
  fast (it is — the child dies at the first sink) and never hardcode anything
  environment-specific in the child code strings.
- Any future change to `ThrowIfFailure` (`src/vm/vm_exec_dispatch.rs:2682`),
  `SinkPop`/`SinkPopAssign` (`:2710`, `:2741`), `compile_try_region`
  (`src/compiler/helpers_control_flow.rs:577`), or the fail-signal-to-Failure
  conversion at sub boundaries touches this matrix; re-run
  `t/try-sink-semantics.t` plus the t/ sink pins above first.
- When the deferred-seq laziness campaign lands, cells P4/P5/P12/P13/P18/Q5/Q6/
  Q9/Q11/Q14/R6/R7 will flip toward raku's stricter behavior — that campaign
  must re-run this file (group A-D stays valid; only divergence notes in the
  deep ticket become obsolete) and sweep the whole whitelist.
