# ADR-0052: A `when`/`default` clause produces its value on the stack, in both branches — retiring the succeed-signal and side-channel value paths

- Status: Accepted (Slice 1 implemented; Slices 2-4 open — see §7)
- Date: 2026-08-20
- Origin: `todo/deep/when-nonmatch-value-outside-map-grep.md` (re-verified
  reproducing on `main` @ `4c58b5f59`, 2026-08-20). The investigation for this
  ADR turned up two further defects in the same mechanism; the small,
  independent one became
  `todo/tickets/given-expr-succeed-branch-leaks-body-stack-value.md`.
- Related: [ADR-0044](0044-listops-are-routines-not-a-syntactic-rewrite.md) —
  the same shape one layer up (a construct given a bespoke transport instead of
  the ordinary value path). No existing ADR covers `given`/`when` value
  production.

## 1. Context

In Raku, `when EXPR { BODY }` is an ordinary value-producing statement. On a
match it is the block's value and it abandons the rest of the enclosing block
(`succeed`); on a *non*-match "the block is not abandoned since the comparison
is false" (`raku-doc/doc/Language/control.rakudoc`) and the clause evaluates to
the failed comparison's falsy result. `default { BODY }` is the always-matching
member of the same family.

mutsu implements this with **three different value transports for one
construct**, and which one is used depends on the branch taken and on which of
several enclosing constructs happens to be executing the body:

1. **The `succeed` control signal.** On a match, `exec_when_op`
   (`src/vm/vm_given_when_ops.rs:447-454`) *peeks* the body's stack top
   (`self.stack.last().cloned()`), stuffs it into
   `RuntimeError::succeed_signal().return_value`, and returns it as an `Err`.
   The body's value therefore exists in **two** places at once — on the stack
   and in the signal. `exec_default_op` (`:499-505`) does the same.
   Roughly twenty `is_succeed()` handlers across the VM and runtime consume
   that signal (`grep -rn 'is_succeed()' src/` — `vm_control_ops.rs`,
   `vm_for_loop_body.rs`, `vm_for_loop_lazy.rs`, `vm_for_loop_intrange.rs`,
   `vm_loop_cstyle_repeat.rs`, `vm_closure_dispatch.rs`, `vm_misc_block.rs`,
   `vm_try_catch_ops.rs`, `vm_react_loop.rs`, `resolution_map_grep*.rs`,
   `methods_dispatch_match.rs`, …), and each is independently responsible for
   discarding the duplicate stack copy.
2. **An interpreter-global side channel.** On a non-match, nothing is pushed
   and no signal is raised; instead `exec_when_op` (`:455-468`) writes
   `Interpreter::when_nonmatch_value` (`src/runtime/mod.rs:1253`). Exactly
   four call sites read it — the inline `map`/`grep`/`first` fast paths
   (`resolution_map_grep.rs:550-562`, `:830-838`;
   `resolution_map_grep_rw.rs:274-282`, `:556-564`) — and only when a *second*,
   compile-time-ish oracle agrees the body's tail is a when-chain
   (`tail_is_when_chain`, `resolution_map_grep.rs:170-176`).
3. **The ordinary stack**, which is what every other consumer of a block's
   value uses — and which for a non-matching `when` holds nothing.

So the correct answer is delivered only down path 2, which exists only for four
callers. Every other way of invoking a when-tail block reads path 3 and finds
whatever the compiler put there instead.

### 1.1 Measured, against `raku` v2026.06 (mutsu @ `4c58b5f59`, debug build)

**(a) The non-match value is wrong everywhere outside `map`/`grep`/`first`.**
(`tmp/when-probe.p6`) These are the origin ticket's three probes, all still
reproducing:

| Probe | raku | mutsu |
|---|---|---|
| `my $b = { when 2 { "two" } }; $b(3).raku` | `Bool::False` | `Nil` |
| `$_ = False; do { when .so { "foo" } }` | `Bool::False` | `Any` |
| `(given 3 { when 2 { "two" } }).raku` | `Bool::False` | `Nil` |
| `sub h { when 99 { "y" }; when 1 { "m" } }; h()` | `0` | `Nil` |

The bytecode shows the mechanism directly:

```
$ mutsu --dump-bytecode -e '$_ = False; my $a = do { when .so { "foo" } }; say $a.raku'
     4: DoBlockExpr { body_end: 10, ... }
     5: GetGlobal(0)
     6: CallMethodMut { name_idx: 2, ... }        # .so
     7: When { body_end: 9 }
     8: LoadConst(3)                              # "foo"
     9: LoadNil                                   # <- the block's value on non-match
    10: SetLocalDecl { slot: 0, explicit_init: true }
```

`compile_block_inline` has no `Stmt::When` arm in `compile_when_tail_stmt`
(`src/compiler/helpers_block_inline.rs:17-101`), so a tail `when` falls to
plain statement compilation and the unconditional trailing
`LoadNil` (`:395-396`) becomes the block's value. (`Any` rather than `Nil` in
the table is ADR-0049's container-default decay at the `my $a` store, not a
second bug.)

**(b) The falsy value itself is reconstructed by a heuristic, not observed.**
`exec_when_op:463` picks `Int 0` when *the matcher* is a type object and
`Bool::False` otherwise. Rakudo's value is instead an artifact of how it
*lowered that particular smartmatch*: a lowering that bottoms out in
`nqp::istype`-style unboxed-int code yields a boxed `Int 0`, everything else a
`Bool`. Measured:

| Source | raku | mutsu |
|---|---|---|
| `$_ = 3;   when 2` | `Bool::False` | `Bool::False` |
| `$_ = 3;   when Str` | `0` | `0` |
| `$_ = Any; when 2` | **`0`** | **`Bool::False`** |
| `$_ = Any; when $m` (matcher in a variable) | `Bool::False` | `Bool::False` |

The third row is wrong today even on the "already fixed" fast path:
`(Any,).map({ when 2 { "x" } })` is `(0,).Seq` in raku and
`(Bool::False,).Seq` in mutsu. mutsu cannot do better than a heuristic here
because `vm_smart_match` returns a bare `bool` — the smartmatch's *value* is
discarded before `exec_when_op` ever sees it.

**(c) The duplicate-transport invariant is already violated — a matching
`when` inside an expression-position `given` leaks a stack value.**
(`tmp/when-stack.p6`)

```raku
say "A: ", (given 2 { when 2 { "two" } });   # raku: "A: two"   mutsu: "twotwo"
say "D: ", (given 3 { default { "d" } });    # raku: "D: d"     mutsu: "dd"
```

`exec_given_op`'s succeed branch truncates to its `stack_base` before pushing
the signal's value (`vm_given_when_ops.rs:224-232`), and its normal exit
enforces "always net exactly +1" (`:243-249`). `exec_do_given_expr_op`'s
succeed branch (`:321-332`) does not truncate — only its `Ok` branch does
(`:314-320`) — so the peeked-and-also-signalled value is pushed twice, and the
enclosing `Say(2)` consumes both, silently dropping the literal `"A: "`. This
is the *same* mechanism seen from the other side: because the value travels
twice, every one of ~20 handlers has to remember to drop one copy, and one
does not. It is a small independent fix and is tracked as its own ticket
(§6), but it is the strongest available evidence that the dual transport is
not maintainable.

### 1.2 Why this is not a point fix — three compilers, three different answers

The obvious repair is "make `exec_when_op` push the falsy value on non-match".
It cannot be done alone, because the three statement-sequence compilers
already disagree about whether a `when` pushes anything:

| Compiler | non-last `Given` | non-last `When`/`Default` | tail `When` |
|---|---|---|---|
| `compile_unit` (`src/compiler/mod.rs:3145-3158`) | `Pop` | `Pop` | nothing appended |
| `compile_sub_body` (`src/compiler/helpers_sub_body.rs:734-736`, `:1147-1149`) | `Pop` | **no `Pop`** | nothing appended |
| `compile_block_inline` (`src/compiler/helpers_block_inline.rs:393-397`) | **no `Pop`** | **no `Pop`** | **trailing `LoadNil`** |

Today `compile_unit`'s `Pop` for a `When` is *never* paired with a push: on a
match the succeed signal escapes before reaching it, and on a non-match nothing
was pushed. `OpCode::Pop` is an unguarded `self.stack.pop()`
(`src/vm/vm_exec_dispatch.rs:2825-2832`) with no stack-base floor, so that
instruction currently underflows into whatever is below — harmless only
because of where the frame boundaries happen to fall. Verified:

```
$ mutsu --dump-bytecode -e '$_=1; when 99 { "a" }; say "tail";'
     4: When { body_end: 6 }
     5: LoadConst(3)
     6: Pop            # <- executes with nothing pushed by the When
```

And the loops truncate per iteration **only when they are collecting a value**
(`vm_for_loop_body.rs:679-695`, `vm_control_ops.rs:521-525`,
`vm_loop_cstyle_repeat.rs:105-109` — all guarded by `if let Some(ref mut coll)
= collected`, i.e. `spec.collect`). A sink-position `for 1..4 { when 2 {…} }`
establishes no stack base at all, so once a non-matching `when` starts pushing,
each iteration leaks one value.

Making the push without fixing all of these turns a silent wrong *value* into a
silent stack *corruption* — the class of bug that shows up three constructs
away from its cause, which is exactly why the origin ticket was reclassified
out of `todo/tickets/`.

## 2. Decision

**A `when`/`default` clause leaves exactly one value on the VM stack, on both
the match and the non-match branch, and that stack value is the only transport
of the clause's value. The `succeed` signal carries control, not a value, and
`Interpreter::when_nonmatch_value` is deleted.**

Four consequences:

### 2.1 `exec_when_op` / `exec_default_op` net exactly +1

- Non-match: push the falsy value (today's side-channel write becomes a push).
- Match: the body already left its value on the stack; **stop peeking it into
  the signal**. `RuntimeError::succeed_signal()` keeps `container_name` (which
  is genuinely control-plane provenance) and drops `return_value` for this
  producer.

The clause then obeys the same contract as every other value-producing
statement, and the existing tail-value machinery applies unchanged.

### 2.2 A body-running construct owns a stack base and truncates to it

Generalize the invariant `exec_given_op:243-249` already states for itself —
*"always net exactly +1 stack value"* — to every construct that executes a
statement range: the `is_succeed()` handlers take the value from `stack_base`
upward instead of from `e.return_value`, and loops truncate to their base at
the end of **every** iteration, not only when collecting. This is what makes
§1.1(c) unrepresentable rather than fixed-in-one-place.

### 2.3 The three compilers agree: a `when`/`default`/`given` statement pushes one value; a non-last one is popped; a tail one is not overwritten

- `compile_sub_body` and `compile_block_inline` gain the non-last `Pop` that
  `compile_unit` already emits, for `When`/`Default` as well as `Given`.
- `compile_when_tail_stmt` grows a `Stmt::When`/`Stmt::Default` arm, and
  `compile_block_inline` suppresses its trailing `LoadNil` when the tail
  statement is one — the value is now genuinely on the stack.
- `OpCode::Pop` gains no floor check; the point is that the emission becomes
  balanced by construction, which a floor check would merely hide.

### 2.4 The falsy value is observed, not reconstructed

`vm_smart_match` gains a value-returning form so `exec_when_op` pushes what the
comparison actually produced. mutsu's smartmatch is not Rakudo's QAST lowering,
so exact parity on Rakudo's unboxed-int artifact is a *deliberate, bounded*
choice rather than an emergent one: reproduce it for the two lowerings roast
and `t/` pin — a type-object matcher (`when Str`) and a literal matcher against
a type-object topic (`$_ = Any; when 2`), both `Int 0` — and return the real
`Bool` elsewhere. The rule is then stated once, in one place, instead of being
a matcher-shape guess at `:463` that the §1.1(b) table shows is already wrong.

Retiring the side channel also retires `tail_is_when_chain`
(`resolution_map_grep.rs:170-176`) and the four fast paths' `tail_is_when`
plumbing: an AST re-inspection that exists purely to disambiguate "the block
produced nothing" from "the block produced nothing *and* it was a when-chain".
Once the clause always pushes, there is nothing to disambiguate.

## 3. Alternatives considered and rejected

**(a) Point-fix each remaining consumer — teach the closure-call boundary,
`DoBlockExpr`, and `DoGivenExpr` to read `when_nonmatch_value` the way the four
fast paths do.** This is the cheapest patch and it would flip all three origin
probes. Rejected: it widens the side channel from four readers to seven or
more, each needing its own `tail_is_when_chain`-equivalent oracle, and it
leaves a construct whose value lives in an interpreter-global field that any
intervening evaluation can clobber. It also cannot fix §1.1(b) or §1.1(c) at
all. This is the band-aid-over-a-wrong-mechanism shape CLAUDE.md's gain/risk
doctrine counts as *risk*, not gain.

**(b) Keep the signal as the value transport and raise a "non-match" signal
too.** Symmetric, and it needs no compiler changes. Rejected because a
non-matching `when` explicitly does *not* abandon the block — a signal would
have to be caught and resumed by the very next statement, which is a control
transfer standing in for a value, and it is precisely the double-transport that
produced §1.1(c). It also keeps ~20 handlers in the value business.

**(c) Give `when` its own opcode result register (a fourth transport, but a
disciplined one).** Rejected: it is `when_nonmatch_value` with a nicer name.
The stack already *is* the value register, and every other statement uses it.

**(d) Do nothing — the `map`/`grep`/`first` shapes are the ones that occur in
real code.** Rejected. `roast/S04-statements/when.t` and `given.t` are both
whitelisted, so the wrongness is currently invisible to CI, which makes it
*more* likely to be built on, not less; §1.1(c) is a silent argument-list
corruption in a shape (`say "label: ", (given … { when … })`) that reads as
completely ordinary Raku; and the `compile_unit` `Pop` in §1.2 is an unbalanced
emission living one frame-boundary accident away from eating a live value.

## 4. Performance

Neutral to slightly positive. The match path currently clones the body's value
(`self.stack.last().cloned()`) into the signal and then discards one of the two
copies; taking it from the stack removes that clone from every `when` match.
The non-match path swaps an `Option<Value>` field write for a stack push —
equivalent. The four fast paths lose an AST walk (`tail_is_when_chain`) per
`map`/`grep`/`first` call with a block body. The added per-iteration
`stack.truncate(base)` in sink-position loops is a length store on an already-
hot local vector.

## 5. Implementation plan

Each slice must stay green on `roast/S04-statements/when.t`,
`roast/S04-statements/given.t` (both whitelisted), and the `t/` when/given
pins — `when-only-block-nonmatch-value.t`, `when-block-value-not-sunk.t`,
`map-when-succeed.t`, `when-value-through-block-local.t`,
`when-succeed-innermost-block.t`, `when-statement-modifier.t`,
`given-when-tail-if-value.t`, `given-when-tail-assign-value.t`,
`when-in-routine-does-not-leak-to-given.t`,
`when-in-deferred-callback-created-inside-sub.t`, `junction-thread-when.t`,
`comp-group-when-gobbled.t`, `rakuast-given-when.t` — plus full CI.

### Slice 1 — stack-base discipline for the constructs that run a body — **DONE**

- Every `is_succeed()` handler establishes a `stack_base` before running its
  range and truncates to it, mirroring `exec_given_op:224-232` / `:243-249`.
  Start with the one that provably does not (`exec_do_given_expr_op:321-332`)
  and audit the rest against the same shape.
- Loops truncate to base after every iteration, not only under
  `spec.collect` (`vm_for_loop_body.rs:679-695` and its three twins).
- No behaviour change intended beyond fixing §1.1(c). Pin: `t/` regression for
  `say "A: ", (given 2 { when 2 { "two" } })` and the `default` twin.
- This slice subsumes
  `todo/tickets/given-expr-succeed-branch-leaks-body-stack-value.md`; if that
  ticket lands first, this slice generalizes it.

### Slice 2 — the compilers agree on the `when` push

- Add the non-last `Pop` for `When`/`Default` to `compile_sub_body` and
  `compile_block_inline`; add the `Stmt::When`/`Stmt::Default` arm to
  `compile_when_tail_stmt` and suppress the trailing `LoadNil` after it.
- Still no runtime change — the emitted `Pop`s pair with the pushes Slice 3
  introduces, so land Slices 2 and 3 together in one PR if CI shows the
  intermediate state is not stack-balanced. (`compile_unit`'s existing `Pop`
  makes the intermediate state *more* balanced, not less, so a split is
  expected to be viable.)

### Slice 3 — the clause pushes, and the signal stops carrying a value

- `exec_when_op` pushes on non-match; both `exec_when_op` and
  `exec_default_op` stop peeking into `succeed_signal().return_value`.
- The `is_succeed()` handlers read the value from the stack instead of the
  signal.
- Delete `Interpreter::when_nonmatch_value` (`runtime/mod.rs:1253`,
  `runtime_init.rs:2450`, `runtime_thread.rs:489`), `tail_is_when_chain`, and
  the four fast paths' `tail_is_when` plumbing.
- Flip `t/when-only-block-nonmatch-value.t` test 11 from `todo` to a plain
  assertion, and add pins for the four §1.1(a) rows.
- Highest blast radius of the three: this is the slice that changes what ~20
  handlers read.

### Slice 4 — the falsy value is the real comparison result

- Value-returning `vm_smart_match`; `exec_when_op:463`'s matcher-shape
  heuristic is replaced by the observed result, with the two Rakudo
  unboxed-int lowerings reproduced deliberately per §2.4.
- Pin the §1.1(b) table, including `(Any,).map({ when 2 { "x" } })` → `(0,)`.
- Retire `todo/deep/when-nonmatch-value-outside-map-grep.md` per the `todo/`
  lifecycle.

## 6. Out of scope

- **The `DoGivenExpr` succeed-branch stack leak (§1.1(c)) as a standalone
  fix.** It is a verified, single-site, add-one-`truncate` defect with its own
  ticket (`todo/tickets/given-expr-succeed-branch-leaks-body-stack-value.md`)
  and is not blocked by this ADR; Slice 1 generalizes it. The two were filed
  together only because the same investigation exposed both.
- `succeed`/`proceed` semantics themselves, and `succeed`'s use outside a
  `when` body. This ADR changes what the signal *carries*, not when it is
  raised or who may catch it.
- The `env_dirty` dual store and the `runtime/methods.rs` slow path that the
  inline `map`/`grep`/`first` fast paths belong to. Retiring the `tail_is_when`
  plumbing removes one of their bespoke oracles; retiring the fast paths
  themselves is a separate campaign.
- ADR-0049's `Nil`-to-container-default decay, which is what turns the
  §1.1(a) `do`-block probe's `Nil` into `Any`. Independent, and already
  designed.

## 7. Implementation status

### Slice 1 — shipped (2026-08-23)

The §1.1(c) single-site defect landed ahead of the ADR as its own ticket
(`bef233807`, `exec_do_given_expr_op`'s missing `truncate`, pinned by
`t/given-expr-succeed-no-double-push.t`); Slice 1 then generalized it:

- **Loops own an unconditional stack base.** `stack_base` in
  `vm_for_loop_body.rs`, `vm_loop_cstyle_repeat.rs` (both the C-style and the
  `repeat` loop), `vm_control_ops.rs`'s `while`, and both
  `vm_for_loop_lazy.rs` variants stopped being `Option<usize>` gated on
  `spec.collect` and became a plain `self.stack.len()` taken at loop entry;
  every iteration-ending arm (`Ok`, `is_succeed`, `is_redo`, `is_next`,
  `is_last`, `leave`) truncates to it, and the C-style loop also truncates
  after its step range. `vm_for_loop_intrange.rs` — the sink-only int-range
  fast path §1.2 named as establishing *no* base at all — gained one.
  `leave`'s own pushed value is still pushed, after the truncation.
- **The audit of the remaining `is_succeed()` consumers** (`grep -rn
  'is_succeed()' src/`) found the rest already correct:
  `exec_given_op`, `exec_do_block_expr_op`, `exec_succeed_barrier_op`,
  `exec_block_local_branch`, the closure-call boundary
  (`vm_closure_dispatch.rs`) and the CATCH handler each establish a base and
  truncate; the `map`/`grep`/`first` fast paths run their bodies through
  `run_reuse`, which clears the stack on entry; `vm_react_loop.rs` and
  `methods_dispatch_match.rs`'s `THREAD` are call boundaries with no statement
  range of their own; and `exec_when_op` / `exec_default_op` keeping the
  body's value on the stack is the Slice 3 subject, not a leak.
- **One further defect of the same shape was found and fixed**: the
  CONTROL-handled branch of `exec_try_catch_op_inner` ran the handler's
  statement range without returning to `saved_depth` afterwards, so a matching
  `when` inside a CONTROL block left the handler body's value behind and it
  became the enclosing block's value —
  `my $x = do { last; CONTROL { when CX::Last { 7 } } }` was `7`, where raku
  yields an undefined value. It now mirrors the normalization its `is_return`
  sibling and the CATCH handler already performed (truncate, push `Nil`).

Pin: `t/when-succeed-stack-base-discipline.t` (the ADR-named `given`-expression
probes, one per loop flavour in both sink and collecting position, and the two
CONTROL cases; 5 of its 16 assertions fail against the pre-change binary).
Note for whoever writes the Slice 3 pins: a *sink*-position loop body with a
top-level `when` is wrapped in a `SucceedBarrier`, which absorbs the signal
before the loop op ever sees it, so the loop's own `is_succeed` arm is only
reachable from the **collecting** form (`compile_stmts_value` emits no
barrier) — and only a match on the *last* iteration leaves observable residue,
since a following iteration's `Ok` arm truncated it away.

Known still-wrong, and deliberately left to Slice 3: a *collecting* loop drops
a matching iteration's value, because the succeed handlers still ignore it —
`do for 1..3 { when 2 { "hit" }; "plain" }` has 3 elements in raku and 2 in
mutsu. Slice 1 changes neither the count nor the values here; it only makes the
abandoned iteration's stack residue go away.

### Slices 2-4 — open

Unchanged from §5.
