# ADR-0059: An `is rw` routine returns a container — retiring caller-side tail re-interpretation

- Status: Accepted (Slices 1-2 implemented — the bare-`is rw`-tail half
  landed 2026-09-01, see `news/2026-09/is-rw-bare-tail-returns-container.md`;
  Slice 3 open)
- Date: 2026-08-22
- Related: ADR-0013 (container interior mutability), ADR-0024 (mainline
  lexicals), ADR-0036 (element-container Pairs from subscripts)
- Addresses: `todo/deep/is-rw-lvalue-return-is-caller-side-ast-reinterpretation.md`

## Context

Raku's `is rw` contract is that a routine hands its caller a *writable
container*: the caller's assignment writes the storage location the routine
returned.

```raku
sub g(\c) is rw { return-rw c<a> }
my %h; g(%h) = 1;                    # %h is {:a(1)}
```

mutsu had no lvalue return at all. `assign_named_sub_lvalue_with_values`
(`src/runtime/builtins_lvalue.rs`) did not call the routine. It took the
callee's **AST tail expression** — `rw_sub_target_expr(&def.body)`, literally
the last `Stmt::Expr`/`Stmt::Return` of the body — and re-evaluated that
expression *in the caller's frame* (`assign_rw_target_expr`).

That reproduces the syntax of the common one-liner and cannot express the
semantics:

- The tail names a **parameter** (`c`, `%c`, `@steps`). The caller's frame has
  no such binding, so the re-evaluation resolves nothing.
- The tail is `return-rw <non-Var>`. `is_explicit_return_rw_target` recognizes
  only `return-rw $var`, so the whole `Call { name: "return-rw", .. }` node was
  handed to the `Expr::Call` arm, which tried to dispatch `return-rw` as an
  lvalue *sub* and failed.
- The tail is **computed** — a conditional, a recursive dispatch to another
  `is rw` multi. There is no single static expression to re-interpret.

All three failed with `X::Assignment::RO: sub 'g' is not rw`; the method form
(`I.in(%h, "a") = 1`) failed with an unrelated "I cannot be parameterized",
because the assignment fell through to the legacy `$obj.name($value)` setter
convention, which called the method with the assigned value as its only
argument.

The consumer that forced the issue is `Crane` — the sole dependency of
`Config::TOML`, the selected TOML battery (`docs/batteries/toml.md`). Every one
of `Crane::In`'s eight `is rw` multi candidates is built on this shape, and
`Crane::Set.set` writes `Crane::In.in(container, @path) = $value`. With no
lvalue return, every `Crane.set` silently did nothing. `Crane` is not an unusual
consumer: returning a writable element is the idiomatic Raku way to write a
path-addressing container library, and `is rw` accessors are ordinary in plain
classes.

## Decision

**An `is rw` routine returns a container, and lvalue assignment writes through
the container the routine returned.** The routine always runs.

The mechanism has three halves, and every piece it needs already existed in the
codebase — none of it is new machinery, it was simply never connected to the
`is rw` return path.

### 1. Production — the compiler emits a container for a `return-rw` operand

`return-rw <expr>` declares that the whole operand denotes a *storage
location*, not a value. Its operand is therefore compiled in the same
container-producing mode a `:=` bind RHS already uses
(`scalar_bind_autovivify` + `bind_terminal`), which routes a subscript through
`IndexAutovivifyLazyTerminal` → `Value::hash_slot_ref` / `Value::array_slot_ref`:

- an existing element is promoted in place to its shared `ContainerRef` cell
  (`Gc<Mutex<Value>>`), aliased by identity, so the eventual write is visible
  through every other reference to that element and survives COW clones of the
  enclosing container;
- a **missing hash key** yields the deferred `HashEntryRef` token — no entry is
  created, so a routine used as a *getter* does not vivify, while a write
  walk-creates the whole path. This is exactly the autovivification Crane's
  recursive descent needs. (Item 1 below generalized that token: its path steps
  now record whether each subscript was positional, so the walk-create makes an
  `Array` where an index addresses one, and an already-promoted-but-empty
  element cell can anchor a path too.)

Container mode also propagates into the **arguments of a nested call inside the
operand** (the `rw_return_operand` compiler flag). `return-rw in(container{@steps[0]},
@steps[1..*])` is a recursive descent: the argument is a link in the lvalue
chain, not a value, so it must alias the real sub-container. This is the
single-dimension twin of what a `MultiDimIndex` call argument already did
unconditionally (`MultiDimIndexBindRef`). The flag is distinct from
`scalar_bind_autovivify` because a call nested in a plain `:=` RHS must keep the
ordinary `is rw` writeback machinery (see `bind_target_direct`).

### 2. Transport — the chain must not be severed by a value read

Two fixes were required for a container to survive a chain:

- **`OpCode::GetLocalDeferred`.** An ordinary `GetLocal` resolves a deferred
  `HashEntryRef` to its current value (`Any` while the key does not exist),
  which is correct for a value read and fatal for a chain. The subscript
  *target* in container mode now reads through `GetLocalDeferred`: the full
  `GetLocal` resolution (binding aliases, shared/atomic storage, env container
  adoption, lazy thunks, `ContainerRef` deref) minus that one step. `GetLocalRaw`
  is not usable here — it is a naked slot read with no env fallback, which a
  method frame's parameter slot depends on.
- **The subscript *index* is an ordinary value read.** `compile_expr_index` left
  container mode on while compiling the index, so `c{@s[0]}` compiled `@s[0]` as
  a bind-ref and passed a `ContainerRef` where the key was wanted. It is now
  compiled with container mode explicitly off (`compile_subscript_index`).

Both were latent bugs of the pre-existing `:=` path, not new. `my %h; my $x :=
%h<a>; my $y := $x<b>` was broken for the same reason.

### 3. Consumption — one write-through helper, two call sites

`assign_lvalue_container` (`src/runtime/lvalue_container_return.rs`) writes a
value through whatever container a routine returned, and reports `None` for a
plain value:

| Returned | Write |
|---|---|
| `Proxy` | `assign_proxy_lvalue` — the user's `STORE` |
| `ContainerRef(cell)` | store into the shared cell |
| `HashEntryRef` | `hash_entry_write` — walk-create the path, insert at the terminal key |

Both lvalue call sites use it:

- **Subs** — `assign_named_sub_lvalue_with_values` now calls the routine first
  (in lvalue mode) whenever it is `is rw` or its tail is an explicit
  `return-rw`, and writes through the container.
- **Methods** — `try_rw_method_container_lvalue`, attempted at the top of
  `assign_method_lvalue_with_values`, before the name-based attribute and setter
  conventions. It also covers a **type-object invocant** (`Crane::In.in(...) = $v`
  is a class-method lvalue), which the instance-only paths reject outright.

## What happens to the old mechanism

`rw_sub_target_expr` / `is_explicit_return_rw_target` / `assign_rw_target_expr`
and the `rw_tail_expr` plan field are **deleted** (2026-09-01). The routine
always runs, and the assignment writes through whatever it returned: the
compiler produces the container for a `return-rw` operand *and* for the bare
tail of an `is rw`/`is raw` routine (`Compiler::rw_tail`), so the runtime
never inspects a body. Whether a routine is rw-capable at all is a
declaration fact — `is rw`, `is raw`, or a body that spells `return-rw`
(`RoutineBodyFacts::uses_return_rw`) — and a routine that is not still runs
before the assignment is refused, as in Rakudo.

The one shape the container-mode tail compile deliberately does not box is
an **attribute** tail (`method x() is rw { $!x }`): `return_rw_container_name`
excludes twigils, so `try_rw_method_container_lvalue` still skips the
attribute-accessor shape (`rw_method_attribute_target`) and the attribute
machinery handles it, as before.

So there is one rule with no stated gap: **the container return owns every
location a routine hands back.**

## Slices

- **Slice 1 (this ADR's implementation, shipped):** the `return-rw` operand,
  its nested call arguments, `GetLocalDeferred`, the subscript-index fix, and
  both lvalue call sites.
- **Slice 2 (`return-rw` half shipped; bare-tail half open):** compile a
  *variable* tail to its container. The `return-rw $x` spelling is done —
  `WrapVarRef` + the new `OpCode::CaptureVarCell` box the variable's own slot
  into the shared cell (`capture_var_cell_inner`, the same capture a List
  literal element gets), `return-rw $a, $b` routes *every* operand through that
  path so the result is a list of containers, and the ordinary `my $v = f()`
  read decontainerizes at `SetLocal`. See
  `news/2026-08/return-rw-produces-first-class-containers.md`; pinned by
  `t/return-rw-container-values.t`.

  **Bare-tail half landed 2026-09-01**
  (`news/2026-09/is-rw-bare-tail-returns-container.md`): the last expression
  statement of an `is rw`/`is raw` routine body compiles through
  `compile_return_rw_arg` (`Compiler::rw_tail` → `compile_routine_tail_expr`),
  on every body-compile path — named sub, method, anonymous `sub`, the
  implicit-try and phaser-block wrappers, and the interpreter carrier's
  recompile (`pending_eval_rw_tail`). A ternary tail compiles each arm in
  container mode. `rw_sub_target_expr`, `is_explicit_return_rw_target`,
  `assign_rw_target_expr` and the `rw_tail_expr` plan field are gone;
  `RoutineBodyFacts::uses_return_rw` is the only body fact the lvalue
  machinery keeps. Along the way the promoted element cell learned its
  array's/hash's `value_type` (the core of ADR-0036 slice 4), and a return
  type constraint now checks through the container it receives.
- **Slice 3 (open):** extend container mode to *every* single-dimension
  subscript call argument, matching what `MultiDimIndex` arguments already do
  unconditionally, and retire the `__mutsu_index_rw_arg_*` snapshot/writeback
  temps in `compile_call_arg_with_escape`. Blocked on read-safety of the array
  half: `array_slot_ref(idx, true)` grows the array past the end, so an
  out-of-bounds index in a *read-only* argument position would vivify. The hash
  half is already safe (a missing key stays a lazy token).

## Alternatives considered

- **Eagerly autovivify each descent level** (`hash_autovivify_cell`, i.e. the
  non-lazy `IndexAutovivify`) so every intermediate is a real container.
  Rejected: it would make a *getter* built on the same routine create the path
  it is looking up. `Crane.in` is a public read API, not only `Crane.set`'s
  internal helper.
- **Keep the AST re-interpretation as the primary and add the container path
  only as a fallback.** Rejected: it leaves two mechanisms disagreeing about
  which one owns a given shape, and keeps the *wrong* one authoritative. It also
  keeps the observable bug that an `is rw` routine's body is never executed.
- **Extend container mode to all call arguments immediately** (Slice 3 now).
  Rejected for this slice on read-safety grounds above, not on principle — it is
  the intended end state.

## Verification

- The ticket's three minimal repros — a parameter-reached element, a recursive
  autovivifying descent, and the method form — all now match `raku`. Pinned,
  with twelve more shapes, by `t/is-rw-lvalue-container-return.t` (15 subtests,
  identical output under `raku`).
- The ticket's headline `Crane` repro matches `raku` exactly:
  `Crane.set(%h, :path["a","b"], :value(1))` produced `{}` and now produces
  `{:a(${:b(1)})}`.
- `Crane` 0.1.2 upstream suite, subtest granularity: **263 ok / 188 not-ok →
  280 ok / 176 not-ok**. `t/set.rakutest` 1 ok → 9 ok, `t/in.rakutest` 5 ok →
  9 ok, `t/get.rakutest` 32 ok → 37 ok. File granularity is unchanged at 3/15
  (raku: 15/15) — see below.
- `Config::TOML` 0.1.3 upstream suite: **77 ok / 259 not-ok → 132 ok / 475
  not-ok**, still 0/19 at file granularity (raku: 19/19). The `not-ok` count
  rises because files that used to abort on the first `from-toml` now run to
  completion — `t/grammar-actions/01-primitives.rakutest` executes 142
  assertions where it previously reached 7. The remaining failures are
  grammar/regex-level and independent of this ADR; the `Crane` dependency was
  one blocker among several.
- Full `make roast` and the `t/` TAP suite both pass.

### What still blocks Crane

The `todo/deep` ticket described this ADR's subject as the single remaining
blocker for the TOML battery. Measuring the suites after the fix shows it was
one of several; the rest are separable:

1. ~~**The deferred vivification token is hash-only.**~~ **FIXED** —
   `news/2026-08/deferred-vivification-path-steps-are-typed.md`. The token's
   path steps are typed (`EntryStep::Key` / `EntryStep::Index`, fed by a new
   `is_positional` field on `IndexAutovivifyLazy`/`…Terminal`), so the
   walk-create builds the container the *next* step asks for; its root is an
   `EntryRoot` — a hash, or the shared cell an empty array element was already
   promoted to. `hash_entry_terminal` returns a located `EntryTerminal` slot
   whose `insert` reuses the ordinary element chokepoints, and the read-only
   `hash_entry_locate` replaced `=:=`'s hand-rolled walk. Crane's `Positional`
   candidates work; its suite is 280 → 283 passing subtests (`t/in.rakutest` 9 → 12).
2. **`X::Crane::PositionalIndexInvalid` is not raised** by `Crane::Utils`'
   classifier multis, and **WhateverCode indices** (`*-0`) do not survive a
   path-addressing descent. Both are recorded in `docs/batteries/toml.md`'s
   work list; neither has been bisected to a standalone repro yet.
