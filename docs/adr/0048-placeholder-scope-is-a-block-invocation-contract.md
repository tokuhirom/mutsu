# ADR-0048: Placeholder scope is a per-construct block-invocation contract, not a per-AST-arm boundary flag

- Status: Accepted (P1, P2, P3 landed; P4-P5 not started)
- Date: 2026-08-20
- Supersedes the framing of: `todo/deep/placeholder-scope-loop-while-block-boundaries.md`
  (and, transitively, the retired `todo/tickets/placeholder-scope-while-loop-not-a-boundary.md`)

## Context

A `$^name` placeholder attaches to the innermost enclosing block that can take a
signature, and that block is then invoked with some number of arguments. mutsu
models only *half* of that: `src/ast.rs`'s `collect_ph_stmt_shallow` /
`collect_ph_expr_shallow` decide, per AST arm, a single boolean — "descend
(transparent) or stop (boundary)" — and where a value has to be bound, each
construct's codegen does it ad hoc.

Two structural consequences follow, and both are visible as user-facing
divergences today:

1. **A boolean has no room for "how many arguments does this construct
   supply".** So `if`/`given` bind exactly one placeholder via a copy-pasted
   `collect_placeholders_shallow(...).find(|n| n.starts_with('^'))` — the
   *first* one — and any further placeholder silently falls through to the
   enclosing block instead of producing raku's arity error.
2. **A boolean has no room for "this body may not take a signature at
   all".** So the rejecting constructs (`loop {}`, `try {}`, phasers, statement
   prefixes, `default {}`) are simply left as transparent, and their
   placeholders leak into the enclosing block's signature.

The same table is additionally hand-maintained in three independent walks
(`collect_ph_stmt_shallow`/`collect_ph_expr_shallow`,
`collect_unattached_ph_stmt`, and `src/placeholder_order.rs`'s
`order_check_stmt`/`order_check_expr` + `check_bare_var_stmt`/
`check_bare_var_expr`), and the value-binding half is copy-pasted at four
codegen sites (`src/compiler/stmt.rs:2088` for `Stmt::If`,
`src/compiler/stmt.rs:3062` for `Stmt::Given`,
`src/compiler/helpers_control_flow.rs:270` and
`src/compiler/helpers_do_expr.rs:177` for the two value-position `if` forms).
`src/compiler/helpers_do_expr.rs:21` (`compile_do_block_expr`) holds the one
rejecting case that *is* implemented, and `src/compiler/stmt.rs:3078` /
`src/compiler/helpers_sub_body.rs:1270` hold a fifth, entirely separate ad-hoc
diagnostic for the bare-`{}` case.

### Correcting the deep finding this ADR replaces

`todo/deep/placeholder-scope-loop-while-block-boundaries.md` (2026-08-18)
concluded that `while`, `loop {}` and bare `{}` have "three genuinely different
rules, not a single shared boundary decision", and warned: "assume the same is
true here until checked one by one. Do not batch-fix by pattern-matching the
existing arms."

A full construct-by-construct audit against real `raku` (2026-08-20, ~45 probes,
transcripts summarised below) **falsifies that premise.** There is exactly one
rule, parameterised by two columns:

> Every `{}` body is a Block. Per construct, ask (a) *may* that Block carry a
> signature, and (b) *what* does the construct pass when it invokes it. A
> placeholder in a body whose answer to (a) is "no" is a compile-time
> `X::Placeholder::Block`; otherwise the body's placeholders are its own
> parameters, and supplying fewer arguments than it declares is the ordinary
> runtime "Too few positionals passed" arity failure.

That is good news: the work is a table plus one shared emitter, not eleven
bespoke investigations. It is still architectural — the table has nowhere to
live in the current model, and populating it changes the real *arity* of
existing blocks — which is why it belongs in an ADR rather than a ticket.

## Evidence

Audited on `main` @ `227e38e4f` with a debug build against system `raku`.
Probe scripts were throwaway (`tmp/ph-probe*.sh`); the table is the result.

### Constructs whose body may take a signature

| Construct | raku supplies | raku observable | mutsu today |
|---|---|---|---|
| routine body, `{...}`/`-> ...` closure *as a value* | caller's args | `{ $^c }(42)` -> 42 | correct |
| `if`/`elsif`/`unless`/`with`/`without` block | 1 — the **raw** condition value | `if 42 { $^c }` -> 42 | correct for one placeholder |
| `if 42 { "$^a $^b" }` | still 1 | `Too few positionals passed; expected 2 arguments but got 1` | prints `42 True` — `$^b` leaks to the enclosing block |
| `while`/`until` block | 1 — the **raw** condition value, re-supplied every iteration | `while 42 { $^c }` -> `42`; `until False { $^c }` -> `False` | prints `True` (a *boolified* condition) **and** leaks the name: `{ while 42 { $^c } }.arity` is 1, raku's is 0 |
| `repeat {} while/until` block | 1 — `Mu` on the first pass, then the condition value | `Mu`, then `True` | `True`, `True` |
| `while COND -> $x { $^c }` | — | `Placeholder variable '$^c' cannot override existing signature` | accepted silently |
| `for` block | N — N elements per iteration, N = the body's placeholder count | `for 1,2 { "$^a $^b" }` -> `1 2` | correct (this is the existing precedent for an N-ary supply) |
| `given`/`with` block | 1 — the topic | `given 5 { $^c }` -> 5 | correct for one placeholder; `given 5 { "$^a $^b" }` prints `5 True` where raku raises the arity error |
| `when` block | **0** | `given 5 { when 5 { $^c } }` -> `Too few positionals passed; expected 1 argument but got 0`; `{ when 5 { $^c } }.arity` is 0 | binds the topic (prints 5) and reports arity 1 |
| bare `{ ... }` **statement** | **0** | `{ $^c }` -> `Too few positionals passed; expected 1 argument but got 0` | ad-hoc die `Implicit placeholder parameters are not available in bare nested blocks` at the mainline; inside a sub it instead leaks arity onto the sub |
| `role` body | 1 — `Mu` at composition | `role R { $^c }; class D does R {}` -> `(Mu)`, runs fine | **over-rejects** with `X::Placeholder::Block` |

### Constructs whose body may **not** take a signature

All of the following are one and the same compile-time error in raku —
`Placeholder variable '$^c' may not be used here because the surrounding block
does not take a signature.` — i.e. exactly what `compile_do_block_expr` already
emits via `method_signature_shared::placeholder_scope_error("block", ph)`:

| Construct | mutsu today |
|---|---|
| `do { }` | correct (the only implemented case) |
| `quietly { }`, `class C { }` | correct |
| `loop { }` (headerless and C-style) | accepted; leaks to the enclosing block |
| `try { }` | accepted; leaks |
| `react { }` | accepted; leaks |
| `once { }` | accepted; leaks |
| `default { }` (incl. inside `CATCH`) | accepted; binds the topic |
| phasers: `BEGIN`, `ENTER`, `LEAVE`, `CONTROL`, `CATCH`, ... | accepted; leaks |
| `race { }` | accepted; leaks |
| `gather`, `supply`, `start`, `sink`, `lazy`, `eager` | accepted, and the placeholder silently reads `Any` (prints empty) — a *third* wrong behaviour, distinct from the leak |
| `module M { }` | accepted; prints `Nil` |

The mainline itself is not signature-capable in either implementation:
`raku -e 'say $^a'` and mutsu both give *"Cannot use placeholder parameter $^a
outside of a sub or block"*.

So mutsu has three different wrong behaviours (leak-to-enclosing-signature,
leak-to-`Any`, ad-hoc die) where raku has one rule, and gets the *supplied
arity* wrong on four constructs it already treats as boundaries.

## Decision

### D1 — Keep inlining control-flow bodies; do not build real `Block` objects

Raku's model is "every body is a Block invoked with arguments". mutsu compiles
`if`/`while`/`given`/`when`/bare-block bodies *inline* into the enclosing
frame's bytecode (`compile_block_inline`), which is why placeholder attribution
had to be reconstructed by an AST walk in the first place.

We keep that. Reifying a `Block` per `if`/`while` body would be a large,
diffuse performance regression on the hottest constructs in the language, for a
feature whose entire observable surface is placeholder attribution and one
error message. **The block-invocation contract is adopted as a *model*, not as
a runtime representation**: we encode "may this body take a signature" and
"what does the construct supply" as compile-time data, and keep emitting inline
code.

### D2 — One classification oracle, consulted by every walk and by codegen

Introduce a single function in `src/ast.rs` that is the *only* place the table
above lives:

```rust
pub(crate) enum ArgSupply {
    /// The enclosing block's own arguments (routine bodies, closure values).
    CallerArgs,
    /// One argument: the raw (un-boolified) condition value.
    Condition,
    /// One argument: `Mu` on the first pass, then the condition value.
    ConditionAfterFirstPass,
    /// One argument: the topic.
    Topic,
    /// N arguments per iteration, N = the body's own placeholder count.
    Elements,
    /// Zero arguments.
    None,
}

pub(crate) enum PlaceholderBodyKind {
    /// No block of its own: descend, placeholders belong to the enclosing scope.
    /// (Statement modifiers: `$^a if $^n`, `$^b.push($_) for @list`, `... given $^n`.)
    Transparent,
    /// A boundary that takes a signature; the construct supplies `ArgSupply`.
    Signature(ArgSupply),
    /// A boundary that may not take a signature: X::Placeholder::Block.
    NoSignature,
}

pub(crate) fn placeholder_body_kind(stmt: &Stmt) -> PlaceholderBodyKind;
// plus an `Expr` sibling for Try/Gather/Supply/Start/Race/DoBlock/...
```

`collect_ph_stmt_shallow` / `collect_ph_expr_shallow`,
`collect_unattached_ph_stmt`, and `placeholder_order.rs`'s
`order_check_stmt` / `order_check_expr` / `check_bare_var_stmt` /
`check_bare_var_expr` each collapse to a thin `match placeholder_body_kind(..)`.
Three hand-mirrored tables become one; the long explanatory comments currently
attached to the `If` / `For` / `Given` / `Whenever` arms move to the oracle,
where they document the whole table instead of one arm.

### D3 — One shared emitter for the bind, and it binds *all* the placeholders

Replace the four copy-pasted `cond_placeholder` blocks with a single compiler
helper:

```rust
fn emit_inlined_body_placeholder_binds(&mut self, body: &[Stmt], supplied: ArgSupply);
```

It collects **all** caret placeholders of the body in `collect_placeholders_shallow`
order (not `.find(first)`), binds as many as `supplied` provides, and — when the
body declares more than `supplied` provides — emits raku's exact runtime
failure, `Too few positionals passed; expected N arguments but got M`. This one
change fixes `if 42 { "$^a $^b" }` and `given 5 { "$^a $^b" }`, and makes
`when` and the bare `{}` statement (both `ArgSupply::None`) correct for free.

### D4 — `while`/`until`/`repeat` supply the raw condition value, per iteration

`Stmt::While` (`src/compiler/stmt.rs:2206`) has no placeholder handling at all
today; `compile_condition_expr` boolifies the condition for the loop jump, so
the only value reachable inside the body is a `Bool`. The fix mirrors
`compile_if_value`'s shape but is *not* a reuse of it: `Dup` the condition
**before** the boolify, and place the bind in the loop-body prologue so it is
re-executed on every pass. `repeat`'s first pass supplies `Mu` (the condition
has not run yet), so `ArgSupply::ConditionAfterFirstPass` seeds the slot with
`Mu` ahead of the loop.

This is the only genuinely new codegen in the plan.

### D5 — An explicit signature wins over a placeholder

`while COND -> $x { $^c }` is `X::Placeholder::Signature`-shaped
("Placeholder variable '$^c' cannot override existing signature"). The
`binding_var.is_none()` guards already scattered through the `if` sites
approximate this by staying silent; promote them to the oracle plus a real
error.

### D6 — The bare `{}` statement stays inlined and gets the arity error, not a real invocation

raku compiles a bare `{ ... }` statement as a genuine Block and *calls* it with
zero arguments, so `{ $^c }` dies at runtime with "Too few positionals passed".
Two ways to match that:

- **(a) Compile it as a real invoked closure.** Exact, but changes bare-block
  compilation wholesale — lexical scoping, `state` re-clone semantics, phaser
  placement, `LEAVE`, `import` scoping — for every bare block in the codebase,
  placeholder or not.
- **(b) Keep inlining; classify it `Signature(ArgSupply::None)`.** *(chosen)*
  D3's shared arity check then produces raku's exact message and exit
  behaviour, replacing the ad-hoc string at `src/compiler/stmt.rs:3078` and
  `src/compiler/helpers_sub_body.rs:1270`.

(b) is chosen because the *only* observable difference between them is the
error text, and (b) produces the right text: a placeholder-bearing bare block
always dies, so no surviving block ever needs the real invocation. If a future
requirement makes bare blocks first-class invokables for unrelated reasons,
this classification stays valid unchanged.

### D7 — Role bodies are signature-capable; module bodies are not

`role R { $^c }` is legal in raku (the body runs at composition with `Mu`);
`module M { $^c }` and `class C { $^c }` are compile-time errors. mutsu has
both backwards for `role` (over-rejects) and `module` (accepts). Classify
`RoleDecl` as `Signature(ArgSupply::None)`-with-`Mu` and `module` as
`NoSignature`.

## Rejected alternatives

- **Fix the constructs one at a time as tickets.** This is what the superseded
  deep finding proposed, on the belief that each construct had its own rule.
  The audit shows they do not, and eleven independent patches to three mirrored
  walks would re-create the very drift that made the current arms diverge.
- **Reify a `Block` per control-flow body (full raku fidelity).** Rejected per
  D1: a large regression on the hottest constructs, buying only error-message
  fidelity that D3/D6 already deliver.
- **Treat this as diagnostics-only (emit errors, leave arity alone).**
  Rejected: the leak is not merely a missing diagnostic. `{ while 42 { $^c } }`
  really has arity 1 in mutsu and arity 0 in raku, so a block that mutsu
  believes needs an argument is a live mis-compilation of the *enclosing*
  block's signature, not just a missing warning.

## Consequences

- Changing the boundary set changes the real arity of any existing block whose
  only `$^name` use sits inside one of the newly-classified bodies. Every call
  site of `collect_placeholders_shallow` (`src/compiler/expr_closure.rs:101`,
  `:111`; `src/compiler/expr_block.rs:120`, `:947`; `src/compiler/stmt.rs:831`,
  `:4122`) must be re-audited for what it assumes about a block's own signature.
  This is the highest-risk part of the change and the reason CI's roast run is
  the acceptance gate.
- Several constructs move from "silently accepted" to "compile-time error".
  Any bundled battery or roast test that happens to contain a stray placeholder
  in a `try {}` / phaser / statement-prefix body will start failing loudly.
  That is the intended outcome, but it should be expected during the first CI
  run rather than treated as a regression.
- After D2 the per-arm comments in `collect_ph_stmt_shallow` become redundant.
  Move them, do not duplicate them: a second copy of the table is exactly the
  failure mode being retired.

## Implementation phases

1. **Oracle + walk unification (no behaviour change). LANDED (PR #6796,
   2026-08-21).** Added `placeholder_body_kind`/`placeholder_body_kind_expr`
   (`src/ast.rs`) and rewrote `collect_ph_stmt_shallow`/
   `collect_ph_expr_shallow`, `placeholder_order.rs`'s `order_check_stmt`/
   `order_check_expr`/`check_bare_var_stmt`/`check_bare_var_expr`, and
   `collect_unattached_ph_stmt`'s `For`-modifier check to consult it, moving
   the scattered per-arm comments into the oracle's doc comments. Verified
   zero behaviour change via `make test` (only pre-existing, environment-
   specific failure: `t/compunit-can-install.t` test 4, unrelated) and a
   targeted run of every `t/*placeholder*.t` file.
   - **`collect_unattached_ph_stmt`/`collect_unattached_ph_expr` stayed
     narrower than the oracle on purpose.** They are a deliberately
     conservative subset detector (documented "false negatives are safe")
     that, unlike the other three walks, never descends an `If`'s branches
     (even for a statement modifier) or a `While`/`When`/`Loop`/`React`/etc.
     body at all — only `For`'s modifier check already had a body-descend
     decision to replace with an oracle lookup. Extending it to the full
     table is left to a later phase; doing so in Phase 1 would have newly
     detected placeholders this walk has never looked for, which is an
     observable behaviour change.
   - **`Expr::DoBlock` is classified `Transparent`, not `NoSignature`, despite
     `do {}` already rejecting a stray placeholder at runtime.** That
     rejection is a wholly separate, unconditional check in
     `compile_do_block_expr`, which exempts a placeholder already "attached"
     to the *enclosing* block's signature — and that attachment only exists
     because the shallow walk treats `DoBlock` as transparent: the parser's
     chained-comparison desugar (`0 <= $^p <= 5`) wraps `$^p` in a synthetic
     `DoBlock`, so `where`/`subset` predicates written that way
     (`t/subset-where-placeholder-chain.t`; broke Cro::Core's `Cro::Port`
     when tried) rely on the leak. See the long note on
     `PlaceholderBodyKind::NoSignature` in `src/ast.rs` for the full
     explanation. Giving `do {}` a real `NoSignature` classification is left
     to whichever later phase untangles this interaction.
2. **Rejecting set. LANDED (PR #6820, 2026-08-21).** Flipped `loop` (headerless and
   C-style — `repeat: false`), `try`, `react`, `once`, `default`, standalone
   `CATCH`/`CONTROL`, every `Stmt::Phaser` kind (`BEGIN`/`CHECK`/`INIT`/
   `ENTER`/`LEAVE`/`KEEP`/`UNDO`/`END`/`PRE`/`POST`), `gather`, and
   `module`/`package`/`grammar` (all three `PackageKind`s, not just `module`
   — raku rejects all three identically) to reject with the same
   `placeholder_scope_error("block", ph)`/`X::Placeholder::Block`
   `do {}`'s existing rejection uses, via the shared
   `Compiler::emit_block_placeholder_die` helper.
   - **`repeat {} while/until` (`repeat: true`) is a same-AST-variant sibling
     that must NOT be included, and an initial pass of this phase wrongly
     lumped it in with `loop {}` before a roast-corpus scan caught it.**
     `Stmt::Loop`'s `repeat: bool` field distinguishes headerless/C-style
     `loop {}` from `repeat {} while/until` — both compile through the same
     AST variant, but per the evidence table above, only the former is
     `NoSignature`; `repeat` is signature-capable
     (`ArgSupply::ConditionAfterFirstPass`, D4/Phase 4's territory) and
     `raku` does not reject a placeholder inside one. This is exactly what
     `roast/S04-statements/repeat.t`'s "placeholders and 'repeat while' mix"
     subtest (from `old-issue-tracker/issues/1283`) pins — a corpus scan of
     `roast/`/`modules/`/`vendor/` for `$^`/`@^`/`%^` usage directly inside
     any of this phase's rejecting constructs turned up exactly this one
     real hit (every other match was either legitimate — inside a nested
     signature-capable block/closure/`where` — or already correctly
     unaffected). Fixed by narrowing the `emit_block_placeholder_die` guard
     (and the oracle classification) to `repeat: false` only; pinned by both
     the roast file staying whitelisted and a new case in
     `t/placeholder-scope-rejecting.t` asserting `repeat` still *accepts* a
     placeholder.
   - **A method's implicit `*%_` (leftover named args) must stay usable
     inside a nested `NoSignature` block, and the first CI run of this PR
     broke exactly that.** `t/placeholder-named-in-method-do.t` already pins
     that `%_` is valid anywhere in a method body, including nested in a
     signature-less `do {}` — `compile_do_block_expr` has always exempted
     `self.lexically_in_method && ph == "%_"` from its stray-placeholder
     check. `Compiler::emit_block_placeholder_die` (the helper this phase
     reuses for every other `NoSignature` construct) had no such exemption,
     so `try {}`/`loop {}`/... newly rejected a legitimate `%_` inside a
     method. This broke every DBIish battery test at once: `DBIish::
     CommonTesting.connect-or-skip` (a method) calls `DBIish.connect(
     $driver-name, |%_)` inside a `try {}`, so the bundled-library gate's
     "Bundled-library test suites" CI job failed with every DBIish backend
     collapsing to `ok=2/109` (including SQLite, which needs no live
     server — proving it was not a service-availability issue) and the gate
     reporting `REGRESSION` against the whitelist for every whitelisted
     DBIish file. Root-caused by reproducing locally: `%_`/`@^`/`$^` weren't
     even considered during the original `roast`/`modules`/`vendor` scan
     above, because that scan only grepped for the caret sigil forms
     (`$^`/`@^`/`%^`) — it missed the *implicit slurpy* placeholder forms
     (`@_`/`%_`), which `collect_unattached_placeholders` also recognizes.
     Fixed by adding the same `lexically_in_method && ph == "%_"` exemption
     to `emit_block_placeholder_die` itself (so every call site gets it for
     free), plus a parallel exemption in `supply_method_call`
     (`src/parser/primary/ident/supply.rs`), which checks
     `collect_unattached_placeholders` directly at *parse* time (no
     `Compiler`/`lexically_in_method` available yet) rather than through the
     shared helper — that one exempts `%_` unconditionally rather than only
     inside a method, a deliberate (documented in that file) narrower gap:
     `supply { %_ }` outside a method should reject per `raku` but no longer
     does, versus the alternative of `supply { %_ }` inside a method
     wrongly degrading into an eagerly-run `DoBlock` and breaking `supply`'s
     async semantics. `@_` is not exempted anywhere (only a method gets an
     implicit `*%_`, never `*@_`). Pinned by four new cases in
     `t/placeholder-scope-rejecting.t` (method-context `%_` accepted inside
     `try`/`loop`/`supply`, and non-method-context `%_` inside `try` still
     rejecting) and by re-running the DBIish/SQLite battery files locally
     (`44-sqlite-memory.rakutest` 108/109 — matching the pre-existing
     baseline — and `25-mysql-common.rakutest`/`34-pg-types.rakutest`
     gracefully SKIPping all subtests again, both previously collapsed to
     `ok=2/109`).
   - **The oracle change alone was not sufficient.** `placeholder_body_kind`/
     `placeholder_body_kind_expr` reclassifying these to `NoSignature` only
     changes what the *shallow walks* (parameter collection, order/redeclare
     checks) do; it does not by itself make the compiler *reject* anything.
     Landing this phase meant separately wiring `emit_block_placeholder_die`
     (or the `collect_unattached_placeholders` check it wraps) into every
     concrete place each construct's body is actually compiled — which, for
     several of these constructs, turned out to be more than one place:
     `BEGIN`/phasers in particular are compiled from up to 6 different call
     sites (statement position via `compile_stmt`, tail/value position via
     `compile_check_phaser_value` from three different callers, a top-level
     mainline *hoisting* pre-pass in `run_toplevel_begin_phasers` that
     bypasses the compiler's `Stmt::Phaser` wrapper entirely by pre-running
     the unwrapped body through the tree-walk-era `eval_block_value`, and
     `ENTER`/`LEAVE`/`KEEP`/`UNDO`/`PRE`/`POST` extraction inside
     `compile_phaser_block_scope`/`compile_pre_phasers`/
     `compile_post_phasers`). The fix pushes the check down into the
     lowest-level shared primitives (`compile_check_phaser`,
     `compile_check_phaser_value`, `compile_pre_phasers`,
     `compile_post_phasers`, the `compile_phaser_block_scope` ENTER/LEAVE/
     KEEP/UNDO loops, plus disqualifying a placeholder-bearing body from
     top-level `BEGIN` hoisting) rather than guarding every call site
     individually.
   - **The statement-prefix group that desugars its body into a real closure
     at PARSE time (`start`, `sink`, `supply`, `lazy`, `eager`) cannot be
     classified via the oracle at all.** By the time `placeholder_body_kind_
     expr` would run, `make_anon_sub` (for `start`/`sink`, via
     `Expr::Call`) or the dedicated `supply`/`.lazy`/`Eager` builders have
     already consumed a bare `{ $^c }` block's placeholder as that closure's
     *own* signature parameter (`Expr::AnonSubParams`/`Expr::Lambda`), so
     there is no `NoSignature`-shaped body left to classify — this is
     exactly why Phase 1 kept `Expr::DoBlock` `Transparent` instead of
     `NoSignature` (see the long note on `PlaceholderBodyKind::NoSignature`
     in `src/ast.rs`), and the same interaction applies to this whole group.
     Each of these five is instead rejected at its own compiler/parser call
     site by re-deriving the same signal `make_anon_sub` used
     (`collect_unattached_placeholders` on the closure's still-intact body)
     and swapping in an `Expr::DoBlock`/direct
     `emit_block_placeholder_die` call instead of compiling the closure:
     `sink`/`start` in `src/compiler/expr_call.rs`, `.lazy`/`Eager` in
     `src/compiler/expr.rs`, `supply` in
     `src/parser/primary/ident/supply.rs` (the only one of the five handled
     at parse time rather than compile time, because `supply {}` always
     builds its own fixed-parameter `Expr::Lambda`, never an
     `AnonSubParams`, so the placeholder is otherwise never looked at again).
     `quietly {}` already had this exact `AnonSub`/`AnonSubParams` → `DoBlock`
     pattern pre-existing (which is how it was already correct per the
     evidence table); Phase 2 only had to extend `sink`'s matching arm to
     cover `AnonSubParams` the same way.
   - **Known gaps left for a follow-up, not blocking this phase:**
     `race { }` (the bare, non-`for` statement-prefix form) has no dedicated
     AST construct in mutsu at all — `race` parses as an ordinary bareword —
     so it is unaddressed. `FIRST`/`NEXT`/`LAST`/`CLOSE` loop-scoped phasers
     and a `CLOSE` phaser nested inside a `supply {}` block are extracted by
     `expand_loop_phasers`/`rewrite_supply_stmt` into synthetic
     `Stmt::Block`/closure shapes before ever reaching a `Stmt::Phaser`-typed
     check, so a placeholder inside one of those specific four kinds still
     leaks rather than rejecting. `PRE {}`/`POST {}` at the true top-level
     mainline are not enforced by mutsu at all yet (a pre-existing gap
     unrelated to this ADR — `PRE { False }` at the mainline does not die
     either) so the placeholder check is correspondingly untestable there;
     `t/placeholder-scope-rejecting.t` pins the sub-body form instead, where
     `PRE`/`POST` are enforced and share the same primitive.
   - Verified via a new `t/placeholder-scope-rejecting.t` (22 cases, one per
     construct/kind) and a clean run of the full local `t/` suite (30788
     tests; the only failure was the pre-existing, environment-specific
     `t/compunit-can-install.t` test 4 already noted under Phase 1).
3. **Shared bind emitter + arity error (D3). LANDED (PR #6897, 2026-08-23).**
   Added `Compiler::emit_inlined_body_placeholder_binds` (plus
   `inlined_body_caret_placeholders`/`inlined_body_binds_supplied_value`) in
   the new `src/compiler/helpers_placeholder_binds.rs`, and routed all five
   copy-pasted single-placeholder-bind sites through it: `Stmt::If`
   (`compiler/stmt.rs`), `compile_if_value`
   (`compiler/helpers_control_flow.rs`), `compile_do_if_expr_bound`
   (`compiler/helpers_do_expr.rs`), `Stmt::Given` (`compiler/stmt.rs`) and the
   value-position `do given` (`compiler/expr_block.rs`). The emitter collects
   *every* caret placeholder of the body (`$^a`, and the `@^a`/`%^a`/`&^a`
   forms the old `.find(|n| n.starts_with('^'))` filter silently skipped —
   raku counts those as positionals too), binds as many as `supplied`
   provides, and emits raku's verbatim
   `Too few positionals passed; expected N argument(s) but got M` (singular
   at N == 1) for the rest. The die is emitted *inside* the body's own
   control-flow region, because the arity failure is raised on invocation:
   `if 0 { "$^a $^b" }` and a non-matching `when` must not raise at all.
   - **`when` and the bare `{}` statement are now
     `Signature(ArgSupply::None)`** (D6), so the shallow walks stop at them
     and `emit_inlined_body_placeholder_binds` raises the zero-supply arity
     error at `Stmt::When`, `Stmt::Block`, and the three tail-block sites
     (`compiler/mod.rs`, two in `compiler/helpers_sub_body.rs`). That retires
     both copies of the ad-hoc `"Implicit placeholder parameters are not
     available in bare nested blocks"` string, *and* fixes the non-tail bare
     block, which previously leaked its placeholder onto the enclosing
     routine's signature entirely unchecked (`sub f { { $^c }; 99 }` gave `f`
     arity 1; raku gives it `()`).
   - **`Stmt::SyntheticBlock` deliberately stays `Transparent`** and is
     excluded from the bare-block check. It is a parser desugar wrapper
     (destructuring declarations, `has` lowering, package meta-statements)
     with no `{ ... }` in the source at all, so a placeholder inside one still
     belongs to the enclosing block. The two ad-hoc-string sites applied their
     check to `SyntheticBlock` as well; that was over-rejection, not a
     behaviour worth preserving.
   - **The `Stmt::If` statement-position site was missing the
     `!is_statement_modifier` guard its two value-position siblings already
     had**, so a NON-tail `if` statement modifier bound the enclosing
     routine's placeholder to the modifier's condition:
     `sub f { say "$^a" if 1; 0 }; f(7)` printed `1` instead of `7` (the tail
     form went through `compile_if_value` and was already correct). Unifying
     the five sites on one emitter made the asymmetry visible; the guard is
     now identical at all of them.
   - **A statement modifier's *modified statement* can itself be a bare
     block, and then that block IS the construct's own** — the first CI-visible
     casualty of D6, caught locally by `t/statement-modifiers.t` and
     `roast/S04-statement-modifiers/{if,unless}.t`. `{ $a = $^x } unless 0`
     parses to an `If` whose `then_branch` is exactly `[Stmt::Block(inner)]`,
     and raku supplies the modifier's value to that block (it prints `0`, and
     `{ $a = $^x } given 69` prints `69`; the parser already lowers those two
     into a `VarDecl` of `^x` at the head of the block —
     `rewrite_placeholder_block_modifier_stmt`). Only a block *genuinely
     nested* inside a construct's braces (`if 1 { { $^a } }`) is a second,
     separately-invoked zero-argument Block, and the two are indistinguishable
     from inside the `Stmt::Block` arm. `Compiler::note_construct_body_block`
     therefore records the body block's *address* (the AST outlives the whole
     compile) for a statement-modifier `If`/`Given`/`For` and for any
     sole-block `While` body, and `is_construct_body_block` makes that one
     node skip the zero-supply check. The loop arms re-note it after
     `expand_loop_phasers`, which rebuilds the body list and so invalidates
     the original address. `Stmt::While` carries no `is_statement_modifier`
     flag, so the far rarer `while C { { $^a } }` gets the same pass — a
     deliberate false negative whose real fix is D4.
   - **Phase 3 required promoting `repeat {} while/until` to its real
     `Signature(ArgSupply::ConditionAfterFirstPass)` classification** — the
     *classification* half of D4 only, no new codegen. Once a bare `{ ... }`
     statement became a zero-argument boundary, a `repeat` nested inside one
     leaked its `$^a` out to that block, which then correctly-but-wrongly
     reported it as a parameter nothing supplies. That is exactly the shape of
     `roast/S04-statements/repeat.t`'s "placeholders and 'repeat while' mix"
     subtest (whitelisted) and of the accepting pin Phase 2 added to
     `t/placeholder-scope-rejecting.t`, so Phase 3 could not land without it.
     Phase 2's note that `repeat` "belongs with D4, not this rejecting set"
     stands — it is still not *rejected*; it is now a boundary whose parameter
     nothing binds yet, which is what D4/Phase 4 finishes.
   - Verified by a new `t/placeholder-scope-signature-capable.t` (36 cases,
     one per row of the signature-capable evidence table plus the
     modifier-over-a-bare-block group) that passes **unmodified under real
     `raku`** as well as under mutsu — including the exact failure text,
     asserted via an `EVAL`/`CATCH` helper rather than `throws-like`'s
     `message` matcher (which mutsu currently accepts and ignores).
   - **Residual divergences Phase 3 deliberately did not chase** (all
     pre-existing, all D4/Phase 4 shaped, none regressed by this phase):
     `{ $a = "$^x $^y" } unless 0` under-supplies without raising, because the
     parser's modifier desugar binds the first placeholder to the condition
     and the rest to `Nil` rather than deferring to D3's emitter;
     `{ @a.push($^x) } for 1, 2` yields `True` per element instead of the
     element (verified pre-existing by temporarily reverting the `Stmt::Block`
     classification); `while` still leaks a placeholder in its body to the
     enclosing routine and mutsu *calls* a `{ ... } while COND` block that
     raku never calls; and a genuinely nested `if 1 { { $^a } }` / `given 5 { {
     $^a } }` prints `True` where raku raises the zero-supply failure.
4. **`while`/`until`/`repeat` raw-condition supply (D4) and the signature
   clash (D5).** (`repeat`'s oracle *classification* already landed in Phase 3
   above; what remains is the per-iteration bind for all three, and `while`'s
   own classification — a placeholder in a `while` body still leaks to the
   enclosing routine.)
5. **Role classification (D7).** (`module`/`package`/`grammar` — the other
   half of D7 — already landed in Phase 2 above.)

## Verification

Each phase lands with `t/` pins built from the audit table above — one file
per column of the table (`t/placeholder-scope-signature-capable.t`,
`t/placeholder-scope-rejecting.t`), each case asserting against the raku
observable recorded here rather than against mutsu's current output.

## Severity and priority

Low urgency: no roast test currently depends on any row of the table, and no
miscompilation of *value flow* has been observed (once a placeholder is bound,
values resolve sensibly). The reason to record the design now is that the audit
that produces the table is the expensive part, and it is done; the phases above
can be picked up independently whenever a maintainability or diagnostics slice
is wanted.
