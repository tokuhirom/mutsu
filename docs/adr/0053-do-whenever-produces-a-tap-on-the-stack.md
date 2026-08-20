# ADR-0053: `do whenever` produces a `Tap` on the stack — retiring the source-variable name bridge

- Status: Proposed (design complete; implementation not started)
- Date: 2026-08-20
- Origin: `todo/deep/whenever-expression-position-needs-real-design.md`
  (re-verified against `main` @ `16a7def3e`, 2026-08-20). The investigation
  **disproves that file's primary premise** — see §1.1 — and relocates the whole
  defect from the parser to the compiler/runtime value path.
- Related:
  [ADR-0052](0052-a-when-clause-produces-its-value-on-the-stack.md) — the same
  shape for `when`/`default`: a value-producing construct given a bespoke
  transport (a control signal, an interpreter-global side channel) instead of
  the ordinary stack. This ADR is that argument applied to `whenever`, whose
  bespoke transport is an *env write to the source variable's name*.
  [ADR-0031](0031-supply-quit-ownership-and-cold-source-tapping.md) — whenever
  source tapping; [ADR-0008](0008-push-based-supply-event-delivery.md) — the
  react drive loop this ADR must give a closable subscription identity;
  [ADR-0028](0028-supply-schedule-on-deferred-tap-delivery.md) /
  [ADR-0043](0043-scheduled-delivery-hop-belongs-to-the-tapped-supply.md) —
  the tap-registration chokepoint, untouched by this ADR.

## 1. Context

### 1.1 The originating finding's premise is wrong: bare `whenever` is not an expression term in Raku

`todo/deep/whenever-expression-position-needs-real-design.md` opens with
"Root cause 1: the parser does not parse `whenever` as an expression term at
all", and its "what a real fix needs" list starts with a parser feature:
"recognize `whenever <supply-expr> [-> <param>] { <body> }` as a valid primary
expression term".

Rakudo rejects exactly that source (raku v2026.06):

```
$ raku -e 'my $s = Supplier.new; react { my $tap = whenever $s.Supply -> $x { }; }'
===SORRY!===
Word 'whenever' interpreted as a listop; please use 'do whenever' to introduce
the statement control word
------>     my $tap = whenever<HERE> $s.Supply -> $x { };
```

`whenever` is a **statement control word**, and the only way to use one in
expression position in Raku is the `do` statement prefix — `do whenever …`.
This is the same rule that makes `my $x = if 1 { 2 }` a compile error and
`my $x = do if 1 { 2 }` legal. Implementing bare `whenever` as a term would be
a mutsu-only dialect extension, not a compatibility fix.

**And the legal form already parses correctly in mutsu.** `--dump-ast` on
`my $tap = do whenever $s.Supply -> $x { … }` shows one construct:

```
VarDecl { name: "tap", expr: DoStmt(Whenever { supply: MethodCall { target: Var("s"), name: "Supply" }, param: Some("x"), body: [ … ] }) }
```

— precisely the `Expr::DoStmt(Box<Stmt::Whenever>)` shape the originating file
proposed building. **No parser work is required.** The entire defect lives in
the compiler and the runtime, in how that already-correct AST is given a value.

### 1.2 The defect, measured

`do whenever` must evaluate to the `Tap` for the subscription it registers.
Every context × supply-shape cell answers wrongly in mutsu (`tmp/wh5.p6`;
mutsu @ `16a7def3e` debug, raku v2026.06):

| Cell | Context | Supply expression | raku | mutsu |
|---|---|---|---|---|
| A | `react { … }` | `$s` (bare `Expr::Var`) | `Tap` | **`Supply`** |
| B | `react { … }` | `$sup.Supply` (`MethodCall`) | `Tap` | **`Any`** |
| C | `supply { … }` | `$s` (bare `Expr::Var`) | `Tap` | **`Supply`** |
| D | `supply { … }` | `$sup.Supply` (`MethodCall`) | `Tap` | **`Any`** |
| E | `react { my $t = do { whenever $s { … } } }` | `$s` | `Tap` | **`Supply`** |

The one shape that works today is an `IO::Socket::Async::Listener` source with
a bare-`Var` supply expression — which is why the sole roast coverage,
`roast/S32-io/IO-Socket-Async.t:332-334` (`isa-ok $listen-tap, Tap`), is
whitelisted and green. It passes through a *different* code path (§1.3, the
`.act()` arms) and is the only reason this defect has stayed invisible.

Consequences beyond the type name: `.close` on the answer dies
(`No such method 'close' for invocant of type 'Any'`), and in cells A/C/E the
**source variable is clobbered** with a value it should never hold.

### 1.3 Root cause: the value transport is a variable *name*, not the stack

Three sites conspire.

**Compiler, `src/compiler/stmt.rs:4158-4177`.** `Stmt::Whenever` emits
`OpCode::WheneverScope { …, target_var_idx }`, where

```rust
let target_var_idx = if self.whenever_bind_target
    && let Expr::Var(name) = supply
{
    Some(self.code.add_constant(Value::str(name.clone())))
} else {
    None
};
```

`target_var_idx` is the **name of the source variable**. It is set only when a
one-shot compiler flag (`Compiler::whenever_bind_target`,
`src/compiler/mod.rs:1281`) is armed *and* the supply expression is literally a
bare variable read. Cells B and D fail this second test — hence `Any`.

**Compiler, `src/compiler/expr_block.rs:701-713.** The `do`-expression arm arms
the flag, compiles the statement, and then *re-reads the source variable* to
obtain the expression's value:

```rust
Stmt::Whenever { supply, .. } => {
    let saved = self.whenever_bind_target;
    self.whenever_bind_target = true;
    self.compile_stmt(stmt);
    self.whenever_bind_target = saved;
    if let Expr::Var(name) = supply {
        self.compile_expr(&Expr::Var(name.clone()));
    } else {
        self.code.emit(OpCode::LoadNil);      // cells B and D land here
    }
}
```

**Runtime, `src/runtime/subtest.rs:521-523`.** In the branch that both `react`
and `supply` bodies actually take, the value written under that name is the
**source Supply**, not a Tap:

```rust
if let Some(name) = target_var {
    self.env.insert(name.to_string(), supply_val);
}
```

Verified by breakpoint (`rust-gdb -batch -ex 'break src/runtime/subtest.rs:522'
-ex 'break src/runtime/subtest.rs:568'`) that cell C — a `supply { … }` body,
not a `react` — hits line 522, not the non-react arm. A `supply` body registers
with a non-empty `supply_emit_buffer`, so the `!self.supply_emit_buffer
.is_empty() || self.react_active > 0` guard at `subtest.rs:464` is true for
*both* legal contexts. The non-react `Supply` arm at `subtest.rs:566-569`,
which does insert the real Tap that `.tap()` returned, is **unreachable from
`do whenever`**; it only serves `whenever` reached from other entry points.
The two `IO::Socket::Async::Listener` arms (`subtest.rs:473`, `:574`) insert
the Tap `.act()` returns — the working exception of §1.2.

**Runtime, `src/vm/vm_scope_ops.rs:280-286.** A Slice-F coherence patch then
pulls that env value into the caller's local slot, because the "binding" was an
env write that never went through the ordinary local-slot store. It is a repair
for the name bridge and exists only because of it.

### 1.4 Why the name bridge cannot be made correct

It conflates two different variables. `target_var_idx` names *the variable
holding the source Supply*; what the program wants named is *a new variable
holding the resulting Tap*. The compiler's own comment
(`stmt.rs:4158-4163`) already records the collision it causes: a **bare**
`whenever $s { … }` statement must not clobber `$s`, because a nested
`whenever` re-tapping the same Supply on a later iteration needs to keep seeing
a Supply. So the mechanism has to be gated by a compiler flag that is set only
inside `do`, and even inside `do` it is wrong — cells A/C/E clobber `$s`
exactly as that comment warns, they simply get away with it because the value
written happens to *be* the Supply.

It also cannot express anything but `my $t = do whenever $s { … }`. A
`MethodCall` supply (the shape the vendored `IO::Socket::Async::SSL` and the
`raku-doc` `ListenSocket` example both use) has no variable to write to at all.

`whenever_bind_target` is additionally a one-shot compiler flag of the kind
that leaks into nested recursive compiles; the design below removes it rather
than auditing it.

## 2. Decision

**`OpCode::WheneverScope` produces the subscription's `Tap` on the stack, like
any other value-producing construct. It never writes a variable.** Four parts:

### D1 — Value on the stack, not a name

Replace `WheneverScope`'s `target_var_idx: Option<u32>` with a
`yields_value: bool` (set by the `do`-expression arm, clear for the statement
form). When set, `exec_whenever_scope_op` pushes the Tap; when clear, it pushes
nothing. `compile_do_stmt_expr`'s `Stmt::Whenever` arm becomes "emit the op with
`yields_value: true`" — no `whenever_bind_target` flag, no `Expr::Var` re-read,
no `LoadNil` fallback. Cells B and D then work *by construction*, because the
value no longer has anything to do with the shape of the supply expression.

The source variable is never written, so §1.4's collision disappears and the
`vm_scope_ops.rs:280-286` locals reconcile is deleted, not adjusted.

### D2 — Every registration path yields a real `Tap`

Assign each `whenever` a `whenever_id: u64` from a process-global counter at
registration time in `run_whenever_with_value`, and return
`Value::make_instance("Tap", { "whenever_id" => id, … })` from *every* arm:

- **react / supply-block arm** (`subtest.rs:464-525`): the subscription does not
  become a tap until the drive loop adopts it, so the id is the only identity
  available at registration time; build the Tap from it.
  **Constraint:** the subscription marker is a 4-element array
  `[source, callback, [LAST…], [QUIT…]]` recognised *structurally* by
  `Interpreter::is_whenever_subscription_marker`
  (`src/vm/vm_react_subscriptions.rs:325-335`), which tests `items.len() == 4`.
  The id must ride in the marker (widen to 5 and update that predicate in the
  same commit) — the marker is a private shape, never user-visible. A separate
  side table keyed by callback identity is the rejected alternative (§3, A3).
  `ReactSubscription` (`src/runtime/subtest.rs:6-46`) gains
  `whenever_id: Option<u64>` so the drive loop can address a subscription.
- **`IO::Socket::Async::Listener` arms** (`subtest.rs:466-477`, `:570-576`):
  keep the Tap `.act()` returns (it already carries `listener-id`, which
  `native_tap`'s close consumes) and stamp `whenever_id` onto it, so close is
  uniform. This keeps `roast/S32-io/IO-Socket-Async.t` green through the change.
- **non-react `Supply` arm** (`subtest.rs:545-569`): keep the Tap `.tap()`
  returns, stamped the same way.

### D3 — `Tap.close` retires the subscription

`Tap` is already a real native class with a `close`/`cancel` implementation
(`src/runtime/native_methods/scheduler.rs:167-230`) that dispatches on which
attribute the tap carries (`listener-id` → `close_async_listener`,
`supplier_id`+`tap_id` → `close_supplier_tap`, `pump_id`, `act_loop_close_ids`,
`upstream_taps`). Add one more arm: `whenever_id` → record the id in a
process-global closed-set, mirroring `set_listener_closed`
(`src/runtime/native_methods/state.rs:880`).

The react drive loop consults that set rather than being mutated directly:

- `drive_react_subscriptions_loop` skips (and marks `sub.done = true`) any
  subscription whose `whenever_id` is closed;
- `adopt_newly_registered_subscriptions`
  (`vm_react_subscriptions.rs:341`) drops a pending marker whose id is already
  closed.

A flag consulted by the loop — rather than a direct removal from the `Vec` the
loop is iterating — is required because the canonical use is
**closing the tap from inside its own callback**. Both documented usages of the
bundled `IO::Socket::Async::SSL` battery depend on it
(`modules/IO-Socket-Async-SSL/lib/IO/Socket/Async/SSL.rakumod`, the module's own
Pod synopses): `$plain-tap.close` inside the tap's callback at `:208-227` — stop
reading the plain socket so the module can upgrade it to TLS, which the Pod
calls out as "the careful handling of the `Tap`" — and `$listener.close` from a
*sibling* `whenever signal(SIGINT)` at `:144-153`. Neither can work until D2 and
D3 land.

### D4 — The statement form sinks its Tap

A bare `whenever $s { … }` statement compiles with `yields_value: false` and
touches no variable. This is what makes D1 safe for the nested-re-tap case the
`stmt.rs:4158-4163` comment describes, and it is why that comment (and the flag
it justifies) can be deleted rather than preserved.

## 3. Alternatives considered and rejected

- **A1 — Parse bare `whenever` as an expression term** (the originating file's
  proposal 1). Rejected: raku rejects that source with a dedicated diagnostic
  (§1.1). Accepting it would make mutsu accept a program rakudo does not, which
  is the private-dialect direction the project rules out; and it does not fix
  any of the five measured cells, all of which use the legal `do` form.
- **A2 — Keep the name bridge but synthesise a fresh target name.** Rejected:
  still an env-keyed side channel, still needs the `vm_scope_ops` locals
  reconcile, and still cannot serve a `whenever` nested inside a larger
  expression. It is ADR-0052's rejected shape one construct over.
- **A3 — Carry `whenever_id` in a side table keyed by the callback `Sub` id
  instead of widening the marker.** Rejected: `nested_react_callbacks` already
  keys on `Sub::id` (`vm_react_subscriptions.rs:356`), and adding a second,
  differently-scoped registry on the same key invites the identity confusion
  that a `Sub` cloned per dispatch would produce. The marker is private and
  matched by exactly one predicate; widening it is a two-line, fully-checked
  change.
- **A4 — Return the source Supply and document the divergence.** Rejected:
  `isa-ok $tap, Tap` is in roast, and `.close` on the handle is the documented
  API for both `ListenSocket` (`raku-doc/doc/Type/IO/Socket/Async/
  ListenSocket.rakudoc:33`) and the upgrade-to-TLS idiom.

## 4. Out of scope

**Statement control words in term position generally.** mutsu is permissive
where rakudo is strict, across the whole family — `my $x = if 1 { 2 }` runs and
yields `2` in mutsu, where rakudo emits "Word 'if' interpreted as a listop;
please use 'do if'". Bare `my $t = whenever … { … }` is the same family, except
that instead of a permissive-but-correct parse it fragments into four
independent statements and binds the bareword string `"whenever"` — the
symptom the originating file opened with. Emitting rakudo's diagnostic for this
family is a **parser-diagnostic** decision, independent of and orthogonal to
this ADR's value-path decision; it should be filed as its own ticket rather
than smuggled in here. Nothing in D1-D4 depends on it, and once D1-D4 land the
only remaining wrong behaviour for the bare form is "mutsu accepts a program
rakudo rejects", not "mutsu computes a wrong value for a legal program".

Also out of scope: `whenever` nested in an arbitrary larger expression (raku
only reaches it through `do`), and any change to the tap-registration
scheduling chokepoint owned by ADR-0028 / ADR-0043.

## 5. Implementation plan

### Slice 1 — the value path (D1, D2, D4)

Stack-yield instead of the name bridge; `whenever_id` assigned at registration;
every arm returns a Tap; `whenever_bind_target` and the `vm_scope_ops.rs`
locals reconcile deleted; the marker widened to 5 elements with
`is_whenever_subscription_marker` updated in the same commit.

Pin: a new `t/react-do-whenever-tap-value.t` covering all five cells of §1.2
(`react`/`supply` × `Var`/`MethodCall`, plus the `do { whenever … }` block
form), asserting `isa-ok $t, Tap` and — for the `Var` cells — that the source
variable is **still a `Supply`** afterwards. Regression guard:
`roast/S32-io/IO-Socket-Async.t` (whitelisted) must stay green, as must
`t/react-do-whenever-tap-coherence.t`,
`t/react-nested-whenever-on-demand-close.t`,
`t/whenever-typed-pointy-param.t` and `t/unicode-yada-ellipsis.t`, whose
comments document the old bridge and will need rewording.

### Slice 2 — `.close` on a whenever Tap (D3)

Closed-id registry; the `native_tap` `whenever_id` arm; the drive loop and
marker adoption both honouring it.

Pin: `t/react-whenever-tap-close.t` — reduce the SSL idiom to a Supplier: close
the tap from inside its own callback on the first value, emit two more, assert
the callback ran exactly once and the react still completes; plus the
sibling-whenever close (close tap A from inside whenever B).

### Slice 3 — residue

`.closed` on a whenever Tap; whether an explicit `.close` fires the
subscription's `LAST` phasers (**measure against raku first** — do not assume);
and re-examining whether the now-unreachable non-react `Supply` arm at
`subtest.rs:566-569` still has a caller once D2 lands.

## 6. Performance

Registration-time cost is one `u64` from a global counter plus one small
`Instance` per `whenever` — negligible beside the `Value::make_sub_owning`
closure clone (plus one per `LAST`/`QUIT` phaser) that the same function
already performs. The drive loop gains one set lookup per subscription per
poll round; it already performs per-round per-subscription source polling.
No steady-state emit-path cost is added.
