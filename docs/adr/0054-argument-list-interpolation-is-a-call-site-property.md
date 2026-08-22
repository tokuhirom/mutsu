# ADR-0054: Argument-list interpolation is a call-site property — retire blind Slip flattening

- Status: Accepted (Slices 1-6 implemented; ADR fully closed out)
- Date: 2026-08-20
- Origin: `todo/deep/blind-slip-flattening-in-fixed-arity-calls.md`
  (re-verified reproducing on `main` @ `b1a9bb8a5`, 2026-08-20; the
  investigation widened the finding well past the ticket's `andthen` repro —
  see §2). One small independent defect turned up on the way and became
  `todo/tickets/fast-binder-skips-too-many-positionals-check.md`.
- Related: [ADR-0021](0021-argument-namedness-is-a-call-site-property.md) —
  the *same* decision for the sibling property. ADR-0021 established
  "named-ness is decided by call-site syntax, never by the value" and fixed it
  with a mint-at-call-site marker plus boundary erasure. This ADR applies the
  identical principle to *slippiness*, which ADR-0021 deliberately did not
  cover: its I4 says what a slip expands **into**, never **whether** to expand.

## 1. Context

### 1.1 The Raku model

Argument-list interpolation is syntax. `f(|EXPR)` spreads `EXPR` into the
argument list; every other argument is one argument, whatever its runtime type.
A `Slip` is an ordinary first-class `List` subtype — it is not a request to
spread. It flattens only where *any* list flattens: into a slurpy parameter, or
into a list-context container.

```
sub g($a)  { ... }   g(Empty)        # one argument, the empty Slip
sub k(*@a) { ... }   k(Empty)        # @a is []      -- slurpy flattening
                     k(|(1,2))       # two arguments -- syntactic interpolation
                     k((1,2).Slip)   # @a is [1,2]   -- slurpy flattening
sub g($a)  { ... }   g((1,2).Slip)   # ONE argument, a 2-element Slip
```

### 1.2 mutsu has two mechanisms and uses the wrong one almost everywhere

**Mechanism 1 — the out-of-band syntactic marker (correct).**
`add_slip_positions_constant` (`src/compiler/mod.rs:2378`) bakes the *positions*
of the `|EXPR` arguments into the constant pool; `OpCode::ExecCallPairs` carries
the index (`src/opcode.rs:1167`) and `spread_slip_positions`
(`src/vm/vm_call_helpers.rs:114`) spreads those positions and only those. Its
own doc comment states the rule: "Argument-list interpolation is a property of
the *syntax*, not of the value." This mechanism is wired to exactly **two**
emission sites (`src/compiler/stmt.rs:2839`,
`src/compiler/helpers_control_flow.rs:839`) — statement-level calls.

**Mechanism 2 — blind value-shape inference (wrong).** For every other call
opcode, `|EXPR` compiles to `OpCode::MakeSlip` and the *value's Slip-ness* is
the only marker the VM has. `append_flattened_call_arg`
(`src/vm/vm_call_helpers.rs:65`) therefore spreads any `Slip`-shaped argument it
sees, and it cannot distinguish `g(|@a)` from `g(@a.Slip)` because by then the
`|` is gone. Consumers:

| Site | Op(s) |
|---|---|
| `src/vm/vm_call_func_ops.rs:781` (`flatten_call_args`) | `CallFunc`, `ExecCall` |
| `src/vm/vm_call_func_ops.rs:998` | `CallOnValue` |
| `src/vm/vm_call_func_ops.rs:1063` | `CallOnCodeVar` |
| `src/vm/vm_call_method_ops.rs:557` | `CallMethod` |
| `src/vm/vm_call_method_mut_ops.rs:49, 372, 579` | `CallMethodMut`, `CallMethodDynamic*` |
| `src/vm/vm_hyper_method_ops.rs:502` | `HyperMethodCall*` |

The compiler *knows* the answer at every one of these sites — a slip argument is
literally `Expr::Unary { op: TokenKind::Pipe, .. }` in `Expr::Call`/method args
and `CallArg::Slip` in `Stmt::Call` — and `src/compiler/expr_call.rs:1538`
already branches on `has_slip`. It just throws the information away instead of
recording it. The comment there is explicit about the consequence:

> `arg_sources_idx` stays None: spreading changes the argument count, so a
> positional source list could not stay aligned.

So the same conflation also *disables the rw-argument source side table* on
every call that contains a `|`, and the VM defensively drops `arg_sources`
whenever its length disagrees with the flattened argument count
(`vm_call_func_ops.rs:783-792`, `:1001-1005`, `:1068-1072`).

### 1.3 The existing band-aid

`preserve_empty_slip_arg` (`src/vm/vm_call_helpers.rs:84`) suppresses the
flattening for an *empty* Slip when the callee name starts with `prefix:<`,
`postfix:<`, `infix:<`, or is one of `andthen` / `notandthen` /
`__mutsu_andthen_finalize`. `src/compiler/helpers_sub_body.rs:1221-1246` carries
a 26-line comment explaining a second, compiler-side dodge for the same root
cause (route named/slip tail statement calls through `ExecCallPairs`, i.e.
mechanism 1, so they escape mechanism 2). Both are name-keyed allow-lists for a
problem that is not about names.

## 2. Measured divergence (mutsu @ `b1a9bb8a5`, debug build, vs `raku` v2026.06)

### 2.1 The high-impact shape: a routine whose tail is a non-firing `if`

In Raku a conditional that does not fire evaluates to `Empty`, i.e. a `Slip`.
This is extremely common code. (`tmp/slip3.raku`)

```raku
sub maybe($x) { if $x { 42 } }        # maybe(0).WHAT is Slip in BOTH
sub show($a)  { say "show got: ", $a.raku }
show(maybe(0));
class C { method m($a) { say "C.m: ", $a.raku } }
C.m(maybe(0));
```

| | raku | mutsu |
|---|---|---|
| `show(maybe(0))` | `show got: Empty` | **`Too few positionals passed; expected 1 arguments but got 0`** |
| `show maybe(0)` (listop) | `show got: Empty` | **same error** |
| `C.m(maybe(0))` | `C.m: Empty` | **`Too few positionals passed; expected 1 argument but got 0`** |

So *any* fixed-arity routine that receives the result of a routine whose tail
conditional did not fire dies with a bogus arity error. The `andthen` case in
the origin ticket is one instance of this family, not the family.

### 2.2 The full argument-shape matrix (`tmp/slip2.raku`)

`sub g($a)`, `class C { method m($a) }`, `my &c = &g`.

| Call | raku | mutsu |
|---|---|---|
| `g $e` (`my $e = Empty`) | `Empty` | `Empty` ✓ *(by accident — see below)* |
| `g(Empty)` | `Empty` | **too few positionals** |
| `g Empty` | `Empty` | **too few positionals** |
| `g $es` (`my $es = ().Slip`) | `$(slip( ))` | `Empty` ✓ |
| `g(().Slip)` | `$(slip( ))` | **too few positionals** |
| `g $ns` (`my $ns = (1,2).Slip`) | `$(slip(1, 2))` | `slip(1, 2)` ✓ |
| `g((1,2).Slip)` | `$(slip(1, 2))` | **`1`** — spread to 2 args, second silently dropped |
| `c(Empty)` | `Empty` | **too few positionals** |
| `C.m(Empty)` | `Empty` | **too few positionals** |
| `C.m((1,2).Slip)` | `$(slip(1, 2))` | **`Too many positionals passed; expected 1 arguments but got 2`** |

✓ means "the callee received exactly one argument", which is what this ADR is
about. The residual `.raku` rendering difference on the two `✓` Slip rows
(`$(slip(1, 2))` vs `slip(1, 2)`) is scalar-itemization display, a separate
concern — do not fold it into this work.

The rows that pass are passing **for the wrong reason**: `g $e` survives only
because a plain source variable is wrapped in `WrapVarRef` before the call
(`--dump-bytecode` confirms), so `append_flattened_call_arg` sees a `VarRef`
rather than a `Slip` and its `match` falls to the `_` arm. Change the argument
expression to anything that is not a bare variable and the protection vanishes.
That is the definition of an accidental invariant.

The `g((1,2).Slip)` row (over-flattening that silently *loses* an argument
rather than erroring) is masked by a second, independent defect: mutsu's fast
binder for simple positional-only signatures skips the "Too many positionals"
check entirely, so `sub g($a); g(1,2)` prints `g:1` where raku dies. Filed
separately as `todo/tickets/fast-binder-skips-too-many-positionals-check.md`;
it matters here because it means over-flattening currently fails *silently*
while under-flattening fails loudly — the bug is wider than the error messages
suggest.

### 2.3 What already works and must not regress

Slurpy binding does **not** depend on call-site flattening. `binding_signature.rs:803-841`
flattens a `Slip` into `*@a` on its own ("Only Slips flatten, since slips always
flatten"), for both the single-argument and multi-argument rules. Verified: with
`sub k(*@a)`, all of `k(|(1,2))`, `k((1,2).Slip)`, `k($z)` and `k(Empty)` already
give raku's answer. **The slurpy half of Raku's Slip semantics is already
correct and independent of this ADR.**

## 3. Decision

**Argument-list interpolation is decided by the call site's syntax, never by the
argument's runtime type.** Every call opcode carries the `|`-argument positions
out of band, exactly as `ExecCallPairs` already does; the VM spreads those
positions and only those. `append_flattened_call_arg`'s value-shape inference,
and the `preserve_empty_slip_arg` name allow-list that patches around it, are
deleted.

### 3.1 Invariants

- **S1 — Marker, not shape.** A `Slip` reaching a call as an ordinary argument
  is one argument. Only a position the compiler recorded as `|EXPR` spreads.
- **S2 — One mechanism.** Mechanism 1 (`add_slip_positions_constant` /
  `spread_slip_positions`) is the only slip-spreading mechanism, on every call
  op. `append_flattened_call_arg` becomes reachable only from
  `spread_slip_positions`, i.e. only for recorded positions.
- **S3 — Expansion rules unchanged.** *What* a recorded slip expands into is
  ADR-0021 I4's business and does not change: `|$pair` / `|%h` yield named
  arguments, `|@l` / `|$list` / `|$slip` yield containerized positionals, `|c`
  replays a Capture's two lanes. `exec_make_slip_op` and `append_slip_item` are
  untouched.
- **S4 — Slurpy flattening stays in the binder.** A `Slip` bound to `*@a` /
  `+@a` flattens at binding time (§2.3), independent of the call site. No call
  op may pre-flatten "on the binder's behalf".
- **S5 — Runtime-invoked calls are positional.** A call with no compiled call
  site (a builtin invoking user code: `map`/`grep`/`sort` callbacks, supply
  taps, `callwith`, `.assuming`) has no `|` and therefore never spreads. Any
  internal caller that *wants* spreading must build the spread argument vector
  itself.

### 3.2 Encoding: extend the existing per-argument side table

Two encodings were considered for carrying the positions to `CallFunc`,
`CallMethod`, `CallMethodMut`, `CallMethodDynamic*`, `CallOnValue`,
`CallOnCodeVar` and `HyperMethodCall*`.

**(a) A second `Option<u32>` field per call opcode**, mirroring `ExecCallPairs`.
Simple and local, and it should fit under the 48-byte `opcode_size_guard`
ceiling (confirm with the guard test before committing to it — do not bump the
limit, per `src/opcode.rs:2117-2131`). But it adds a fetch/decode slot to the
hottest instructions in the VM for information that is `None` on the
overwhelming majority of call sites, and it leaves the `arg_sources` misalign
problem (§1.2) unsolved.

**(b) Fold it into `arg_sources_idx` — chosen.** That side table already exists
on `CallFunc` / `ExecCall` / `CallMethod` / `CallOnValue` / `CallOnCodeVar`, is
already one entry per argument position, and already carries a two-shape
per-entry encoding (`Str(name)` for a plain source, `Pair(name, Int(slot))` for
a slotted one, `Value::NIL` for none — `src/compiler/mod.rs:2329-2370`,
decoded by `decode_arg_sources`, `src/vm/vm_call_helpers.rs:174`). Adding a
third entry shape for "this position was written `|EXPR`" costs **zero opcode
bytes** and turns the current mutual exclusion between slip args and rw-arg
source tracking into cooperation: the decoder spreads the source list in
lockstep with the argument list, so the defensive "lengths disagree → drop the
whole table" fallbacks disappear instead of being extended.

Concretely, `add_arg_sources_constant` gains the argument syntax alongside the
source name (it must take the pre-lowering `&[Expr]` / `&[CallArg]`, which the
existing call sites already have), emits `Some(..)` whenever *any* entry is a
slip even if every source name is `NIL`, and `decode_arg_sources` returns the
slip positions next to the names. `add_slip_positions_constant` and
`ExecCallPairs`'s dedicated `slip_positions_idx` collapse into the same table in
a later slice (§4, Slice 4) — one descriptor per call site rather than two
parallel constants.

The four opcodes with no `arg_sources_idx` today — `CallMethodDynamic`
(`src/opcode.rs:1139`), `CallMethodDynamicMut` (`:1146`), `HyperMethodCall`
(`:1747`) and `HyperMethodCallDynamic` (`:1759`) — gain one, which is the same
`Option<u32>` cost as option (a) but only on those four, none of which is on a
hot path.

### 3.3 Rejected alternatives

1. **Keep blind flattening, widen `preserve_empty_slip_arg`.** This is the
   status quo's own escalation path and it cannot work: the allow-list is keyed
   on the *callee's name*, and the bug is a property of the *call site*. It
   also only ever addresses the empty-Slip half (§2.2 shows non-empty Slips
   corrupting arity in both directions). Per the project's gain/risk rules this
   is precisely the "correct only under an incomplete static analysis" shape
   that goes flaky.
2. **A distinct runtime representation for an "argument slip"** (a second Slip
   repr, the way `Pair`/`ValuePair` split named-ness). Rejected: `Slip` is a
   user-visible Raku type that any expression may legitimately produce and
   return, so unlike the Pair flavours there is no minting chokepoint to
   restrict — a user sub returning `Empty` would have to be prevented from
   producing the marker repr, which is impossible. ADR-0021 could take the
   in-band route only because it could make minting a call-site privilege; that
   is not available here. Same argument kills "compile `|EXPR` to a `Capture`
   and spread Captures" — `f($c)` with a user-held `Capture` must not spread.
3. **Teach the runtime to consult the callee's signature** (spread only into a
   slurpy). Rejected: observably wrong (`k(|(1,2))` and `k((1,2).Slip)` must
   both give `[1,2]` for `*@a`, but `g(|(1,2))` must spread and
   `g((1,2).Slip)` must not, for the *same* callee shape), it makes argument
   count depend on the resolved candidate before dispatch has resolved one, and
   it is unavailable for `CallOnValue`/`CallOnCodeVar` where the callee is a
   runtime value.
4. **Two-lane argument plumbing now** (thread `(positionals, nameds, slips)`
   structurally through every dispatch signature). Rejected for the same reason
   ADR-0021 rejected it: it touches every dispatch/binding signature, the
   light-call caches and the JIT helpers for no observable gain over S1–S5,
   and nothing here forecloses it later.

## 4. Phasing

Each slice is independently shippable and gated by CI (`make test` + `make
roast`); the roast suite is the comprehensive net per repo policy.

**Slice 0 — pins first (no behaviour change).** The `|`-interpolation
regression net already exists and is good: `t/slip-arg-flatten.t` (29 cases,
including the cold-vs-warm light-call cache and multiple `|` args in one call).
What it does not cover is a **fixed-arity** callee — every one of its cases
binds into a slurpy or a list, which is why the bug survived it. So add
`t/slip-slurpy-binding-is-independent.t` (§2.3, currently passing) now, and
extend `t/slip-arg-flatten.t`'s lines 75-83 "syntax, not value" block to the
expression call paths as `t/slip-value-argument-is-one-argument.t` (§2.1/§2.2,
currently failing — it lands *with* Slice 2/3, not before).

**Slice 1 — the compiler side of the descriptor.** Extend
`add_arg_sources_constant` with the slip entry shape and emit it from the
`has_slip` branches that currently discard the information
(`src/compiler/expr_call.rs:1538-1561` and the method/`CallOn`/hyper emission
sites). `decode_arg_sources` learns the shape and returns the positions. No VM
behaviour change yet — the flattening path still runs — so this slice is green
on its own.

**Slice 2 — flip the function path.** `flatten_call_args`
(`vm_call_func_ops.rs:781`) and the `CallOnValue`/`CallOnCodeVar` loops
(`:998`, `:1063`) stop inspecting value shape and spread the decoded positions
instead. Delete the `name == "val"` special case (it exists only to opt *out* of
blind flattening) and `preserve_empty_slip_arg`'s operator/`andthen` allow-list.
Fix the `arg_sources` length-mismatch fallbacks to spread the source list in
lockstep rather than dropping it. Acceptance: §2.1 and §2.2's function rows
match raku; `t/tail-stmt-call-named-value.t` and the `andthen`/`orelse` pins
stay green **without** the allow-list.

**Slice 3 — flip the method and hyper paths.** Same change at
`vm_call_method_ops.rs:557`, `vm_call_method_mut_ops.rs:49, 372, 579`,
`vm_hyper_method_ops.rs:502`, adding `arg_sources_idx` to the four opcodes that
lack it (`CallMethodDynamic`, `CallMethodDynamicMut`, `HyperMethodCall`,
`HyperMethodCallDynamic`). Acceptance: the
method rows of §2.2 match raku. `append_flattened_call_arg` now has exactly one
caller (`spread_slip_positions`) and can be inlined into it.

**Slice 4 — collapse the two constants.** Retire
`add_slip_positions_constant` / `ExecCallPairs::slip_positions_idx` in favour of
the unified descriptor, so a call site has one side table describing its
arguments' syntax. Update `stack_args_have_slip`
(`vm_call_helpers.rs:101`) — the light-call/OTF cache bypass — to key off the
compile-time descriptor rather than probing the stack for Slip-shaped values.
This is also a small **perf win**: a call whose ordinary argument merely
evaluated to a `Slip` currently forfeits the light-call cache for no reason.

**Slice 5 — compiler-side dodge cleanup.** With Slice 2 landed, re-evaluate the
`compile_tail_stmt_call_value` routing (`src/compiler/helpers_sub_body.rs:1221-1246`):
its slip-motivated half is subsumed. Keep whatever of it is still needed for
`keep_value` (tail-position value production) and shrink the comment to that.

**Slice 6 — internal-caller audit (S5).** Sweep the runtime for sites that hand
a synthesized `Slip` to `call_*_with_values` expecting it to spread
(`grep -rn 'Value::slip\|slip_arc' src/runtime/ src/builtins/`), and make each
build the spread vector itself. These have no compiled call site, so Slices 2-3
change nothing for them mechanically — the sweep is to confirm none was relying
on the old behaviour via a compiled trampoline.

## 5. Consequences

- The Raku rule "interpolation is call-site syntax" holds on every call path,
  by the one mechanism the codebase already declared correct, instead of on the
  two statement-call paths by one mechanism and on every other call path by
  another.
- Two name-keyed band-aids (`preserve_empty_slip_arg`, the `val` special case)
  and the mutual exclusion between `|` arguments and rw-source tracking are
  deleted rather than extended.
- A very common Raku shape — passing along the result of a routine whose tail
  conditional did not fire — stops dying with a bogus arity error (§2.1).
- Code that leaned on mutsu's wider-than-spec `f(@a.Slip)` spreading starts
  passing one argument. That is the intended semantic change; `flatten_call_args`'
  own doc comment already flags the current behaviour as out of spec. Expect
  fallout in CI, which is the safety net working.
- The light-call/OTF cache gets *more* eligible call sites (Slice 4), not fewer.
- No JIT work: `vm_jit_compile.rs:465-479` routes `CallFunc`/`CallMethod`
  through the same VM helpers, so the JIT inherits the fix.

## 6. Test plan

- **`t/slip-arg-flatten.t` is the existing net and the model.** Its lines 75-83
  already assert this ADR's rule for the statement-call path — comment verbatim:
  "interpolation is a property of the syntax, not of the value", pinning
  `is-deeply |( ('a','a') )` as spreading and `is-deeply @s.Slip, @s.Slip, 'name'`
  as three arguments. Slices 2-3 make the *expression* call paths agree with the
  pin that already exists for the statement path. Its 29 cases (mixed `|`/Slip
  args, multiple slips, cold-vs-warm light-call cache, statement-level slipping,
  `(@s.Slip if $yes)`) must all stay green; every one of them binds into a
  slurpy or a list, so §2.3 says they should — if a case flips, the diagnosis is
  a broken slurpy/list path, not an intended semantic change.
- New: `t/slip-value-argument-is-one-argument.t` (§2.1 + §2.2 matrix across
  function / method / `CallOnValue` / `CallOnCodeVar` / hyper call shapes — the
  gap `slip-arg-flatten.t` does not cover, because every case there has a
  slurpy callee), `t/slip-slurpy-binding-is-independent.t` (§2.3, so a later
  slice cannot "fix" a regression by re-adding call-site flattening).
  `t/slip-value-argument-warm-cache.t` (Slice 4: 50 repeated calls each of a
  Slip-valued single-arg call site, a `|EXPR`-spreading sibling site, a
  slurpy callee, and the "non-firing tail `if`" shape — pins correctness of
  the collapsed `arg_sources_idx` descriptor and the compile-time
  `call_has_slip` cache gate under a warm light-call cache, not just on the
  first cold call).
- Must stay green: `t/slip-listop-args.t`, `t/capture-slip.t`,
  `t/hash-spread-slip.t`, `t/multi-slip-otf.t`, `t/hyper-method-slip-result.t`,
  `t/say-slip-nonflatten.t`, `t/await-slip-flatten.t`,
  `t/tail-stmt-call-named-value.t`, `t/andthen-roast-regressions.t`,
  `t/andthen-orelse-instance-rhs.t`, `t/reduce-notandthen.t`,
  `t/pair-positional-arg.t`, `t/slip-array-of-pairs-is-positional.t`,
  and `.subst(|(:g), ...)` adverb promotion.
- Full roast delegated to CI.

## 7. Implementation status

- [x] Slice 0 pins — folded into the Slice 1/2 PR: `t/slip-arg-flatten.t` was
  already the regression net; `t/slip-value-argument-is-one-argument.t` adds
  the fixed-arity-callee matrix (§2.1/§2.2) this ADR set out to fix.
- [x] Slice 1 compiler descriptor — `add_arg_sources_constant`
  (`src/compiler/mod.rs`) gains the third entry shape (`Value::TRUE`) for a
  `|EXPR` position; the `has_slip` branch in
  `src/compiler/expr_call.rs` now feeds it instead of hardcoding `None`.
  `decode_arg_slip_positions` (`src/vm/vm_call_helpers.rs`) decodes the
  positions. Method/`CallOnValue`/`CallOnCodeVar` emission sites already called
  `add_arg_sources_constant` unconditionally, so they picked up the new shape
  for free; the four opcodes that don't carry `arg_sources_idx` yet
  (`CallMethodDynamic`, `CallMethodDynamicMut`, `HyperMethodCall`,
  `HyperMethodCallDynamic`) still don't — that part of Slice 3 is unstarted.
- [x] Slice 2 function path — `spread_call_args_by_syntax`
  (`src/vm/vm_call_helpers.rs`) replaces `flatten_call_args` (deleted) at the
  three named sites (`vm_call_func_ops.rs`'s `CallFunc`/`CallOnValue`/
  `CallOnCodeVar` handlers) **and** `exec_exec_call_op`'s `ExecCall` handler
  (`vm_call_exec_ops.rs`), which had its own hand-duplicated blind-flatten loop
  not mentioned by name in §4 but is the same mechanism (§1.2's consumer
  table lists it alongside `CallFunc`) and is what a bare listop-style
  statement call compiles to when the callee isn't in the statement-call
  allowlist. The `val`-name special case and the length-mismatch
  "lengths disagree -> drop the whole table" fallback are gone from these four
  sites, replaced by the lockstep expansion `spread_call_args_by_syntax`
  builds directly (one narrow post-sanitize length re-check remains in
  `exec_call_on_code_var_op`, guarding only against the synthetic
  callsite-line marker `sanitize_call_args_owned` can strip — not a
  slip-alignment concern). `preserve_empty_slip_arg` itself is NOT deleted:
  it's still called from the method paths (Slice 3), so removing it now would
  break the build; it becomes single-caller once Slice 3 lands.
  A latent dependency on the old *wider-than-spec* behaviour turned up in the
  builtin `grep`/`map` listops (`src/runtime/builtins_collection_mapgrep.rs`):
  both relied on the call op to have already flattened a Slip-valued list
  argument, since their own item-flattening `match` had no `Slip` arm. Per
  §2.3 a `Slip` must still flatten into their slurpy `+@list` regardless of
  call-site syntax, so both now flatten a `Slip` argument unconditionally
  (ahead of the `single_arg_rule` gate, since Slip flattening is not subject
  to that rule) — this is a Slice 2 fix, not a regression of §2.3.
  Acceptance verified: §2.1 and §2.2's function/listop/code-variable rows now
  match `raku` (dual-oracle checked); `t/slip-arg-flatten.t` and every "must
  stay green" file in §6 stay green without the allow-list.
- [x] Slice 3 method / hyper paths — landed. `CallMethod` (`vm_call_method_ops.rs`)
  and `CallMethodMut` (`vm_call_method_mut_ops.rs`) now call
  `spread_call_args_by_syntax` instead of unconditionally flattening every
  Slip-shaped argument, exactly mirroring the Slice 2 function-path change
  (and, for `CallMethodMut`, now feeding the *post-spread* lockstep source
  list to `set_pending_call_arg_sources` instead of the pre-spread one, so
  `is rw` source tracking and `|` spreading cooperate instead of the old
  defensive length-mismatch fallback silently dropping the source table).
  `CallMethodDynamic`, `CallMethodDynamicMut`, `HyperMethodCall` and
  `HyperMethodCallDynamic` gained the `arg_sources_idx` field §3.2
  anticipated (baked by the same `add_arg_sources_constant` call the scalar
  method sites use) and now spread only the recorded `|EXPR` positions too;
  none of the four tracks rw-arg sources (they never did), so their VM sides
  pass `None` for the decoded-sources slot and discard
  `spread_call_args_by_syntax`'s returned source list. `HyperMethodCallDynamic`
  previously did not flatten a Slip argument AT ALL (neither by shape nor by
  syntax), so this slice is also a genuine fix for `@a>>.$name(|@x)`, not
  just a parity change. The method row of `t/slip-value-argument-is-one-argument.t`
  (`C.m(maybe(0))`) is un-todo'd and the file grew matching cases for
  `CallMethodMut`/`CallMethodDynamic`/`CallMethodDynamicMut`/`HyperMethodCall`/
  `HyperMethodCallDynamic`, plus `|EXPR`-still-spreads pins for a fixed-arity
  and a slurpy method — all 23 cases dual-oracle verified against `raku`.
  `preserve_empty_slip_arg` is deleted outright (its two callers were both in
  this slice's scope); `append_flattened_call_arg` now has two remaining
  callers (`spread_slip_positions`, used by `ExecCallPairs`'s still-separate
  mechanism 1, and `spread_call_args_by_syntax`, used by every other call op)
  rather than the single caller §4 anticipated — Slice 4's constant collapse
  is what reduces that to one, so the inlining note there is deferred to
  that slice.
- [x] Slice 4 constant collapse + cache gate — `ExecCallPairs`'s dedicated
  `slip_positions_idx` (a constant array of bare integer positions, decoded
  by the now-deleted `spread_slip_positions`) is gone; the opcode field is
  renamed `arg_sources_idx` and carries the SAME per-position `TRUE`/`NIL`
  descriptor shape `CallFunc`/`CallMethod`/etc. use (built by the new
  `Compiler::add_call_arg_sources_constant(&[CallArg])`, the `CallArg`-list
  counterpart of `add_arg_sources_constant`; `ExecCallPairs` never tracked
  rw-arg sources, so it only needs the `TRUE`/`NIL` slip marker, not the
  `Str`/`Pair` name entries the `Expr`-list callers also encode).
  `exec_exec_call_pairs_op` (`vm_call_exec_ops.rs`) now calls the same
  `spread_call_args_by_syntax` every other call op uses (passing `None` for
  `decoded_sources` and discarding the always-`None` returned source list),
  so a call site carries exactly one syntax descriptor and one spreading
  mechanism instead of two. This gave `append_flattened_call_arg` its
  anticipated single call site, so it is now inlined directly into
  `spread_call_args_by_syntax`'s loop (its dead `preserve_empty_slip`
  parameter — always `false` at both of its two former call sites — is
  dropped in the same pass).

  The cache-gate half: `stack_args_have_slip` (`vm_call_helpers.rs`) no
  longer takes `&self`/`arity` and probes the stack; it is now
  `Self::stack_args_have_slip(code, arg_sources_idx) -> bool`, a thin
  wrapper over `decode_arg_slip_positions(code, arg_sources_idx).is_some()`
  — a call site with no `|EXPR` position resolves this in O(1) with zero
  allocation, since `arg_sources_idx` is `None` on the common call. In
  `exec_call_func_op` (`vm_call_func_ops.rs`) this is now computed ONCE per
  call, as `call_has_slip`, and reused at all three cache-eligibility
  checks: the ultra-fast positional-light-cache fused scan (which
  previously ran its own separate `matches!(view, ValueView::Slip(_))`
  probe over the stack args inline — removed, since it was the same
  value-shape bug the ADR targets, just not routed through the named
  helper), the named light-call cache check, and the OTF-compiled-function
  cache check. `CallOnValue`/`CallOnCodeVar` have no light-call cache (they
  always spread via `spread_call_args_by_syntax` unconditionally), so no
  change was needed there.

  Verified: a positional-only user sub called 2000 times in a loop with a
  Slip-VALUED (non-`|`) argument (`g(@a.Slip)`) shows
  `function-full-resolve total=1` under `MUTSU_JIT=off MUTSU_VM_STATS=1`
  (one cold resolve, the rest warm-cache hits) both before and after this
  slice — confirmed the OLD runtime-shape check by temporarily reintroducing
  it: the existing `function-full-resolve`/`function-fallback`/
  `function-carrier`/dual-store/JIT counters do NOT discriminate the
  light-call-cache-hit path from the heavier `call_compiled_function_named`
  path for a plain compiled top-level sub with no env/multi-dispatch
  involvement in either case, so no existing `MUTSU_VM_STATS` counter can
  observe this specific cache-eligibility win directly; the correctness of
  staying cache-eligible under a warm cache is instead pinned by
  `t/slip-value-argument-warm-cache.t` (50 repeated calls each, mixing a
  Slip-valued single-arg call site, a `|EXPR`-spreading sibling call site,
  a slurpy callee, and the ADR's motivating "non-firing tail `if`" shape,
  so the two call-site shapes cannot get confused once their cache entries
  are warm). A `perf stat`/wall-clock A/B is the only observable signal for
  the raw speedup and is not suitable for a deterministic CI-gated test per
  repo policy (`docs/flaky-test-policy.md`).
- [x] Slice 5 compiler dodge cleanup — `compile_tail_stmt_call_value`
  (`helpers_control_flow.rs`) and its `Stmt::Call` tail-position call site
  (`helpers_sub_body.rs`) keep the SAME routing (positional-only args reuse
  `Expr::Call`; named/slip args route through `ExecCallPairs { keep_value:
  true }`) — Slice 4 did not change which call op tail-position dispatch
  compiles to, only what `ExecCallPairs` decodes internally. Both doc
  comments are rewritten to drop the "must NOT go through
  `call_args_to_expr_args`... `append_flattened_call_arg`... silently
  dropping an `Empty`-valued positional argument" justification: that
  concern is gone now that `CallFunc` tracks `|EXPR` positions by call-site
  syntax exactly like `ExecCallPairs` does (Slices 1-4), so
  `ExecCallPairs`'s syntax-accurate slip tracking is no longer a
  distinguishing reason to prefer it over `Expr::Call` for this shape. The
  ONLY remaining reason for the special-cased routing is `keep_value`
  (tail-position value production): `Stmt::Call`'s plain statement op
  (`ExecCall`/`ExecCallPairs { keep_value: false }`) discards its result, so
  a tail call with named/slip args still needs the `keep_value: true`
  routing or the routine would return its topic instead of the call's value
  (JSON::Marshal's `to-json($ret, :$sorted-keys, :$pretty)` regression this
  guards against). No behaviour change; `t/tail-stmt-call-named-value.t`
  stays green.
- [x] Slice 6 internal-caller audit (S5) — swept every
  `Value::slip`/`slip_arc` construction in `src/runtime/` and
  `src/builtins/` (`grep -rn 'Value::slip\|slip_arc' src/runtime/
  src/builtins/`, ~20 sites). Every one of them constructs a `Slip` as a
  RETURN VALUE (a `.Slip`/`Slip.new`/`slip()` coercion or constructor, the
  `andthen`/`notandthen`/`succeed`/`leave`/`make` "return `Empty`" control-
  flow signal value, or a recursive value-transform like hyper-prefix
  mapping or Proxy resolution preserving Slip-ness through a nested
  structure) — never as a synthesized argument handed to a
  `call_*_with_values` function expecting the call machinery to
  auto-spread it. No internal (non-compiled) caller was relying on the old
  blind-flattening behaviour via a compiled trampoline: S5's runtime-
  invoked calls (map/grep/sort callbacks, supply taps, `callwith`,
  `.assuming`, etc.) already build their argument vectors directly as
  `Vec<Value>`/`&[Value]`, matching the ADR's expectation. No code change
  was needed for this slice; it is a clean audit result, not a skipped
  check.
