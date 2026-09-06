# ADR-0067: A routine hands back the container it was *given* — raw arguments, raw invocants, and the subscript step through an object

- Status: Proposed (Slices 1, 2, 3a, 4 and 5 implemented 2026-09-05, Slice 3b,
  the E6 rw-attribute-accessor producer and the returned-container consumers
  2026-09-06; Slice 3 was re-scoped
  into 3a/3b on 2026-09-05 after measurement, and 3a's E6 row split off again
  into that producer; Slice 4 absorbed two of Slice 5's three acceptance rows,
  again after measurement; Slice 3b shipped the *named-receiver* half of the
  arrival direction and split its subscript-receiver rows (I3/K3) off into a
  producer slice of their own, which is the only slice still open)
- Date: 2026-09-05
- Related: [ADR-0059](0059-is-rw-routines-return-a-container.md) (an `is rw`
  routine returns a container), [ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md)
  (element-container Pairs), [ADR-0013](0013-container-interior-mutability-cellvalue.md)
  §7 (interior-mutability prerequisite — solved),
  [ADR-0001](0001-gc-strategy-and-phasing.md) §7 (Track B is no longer
  GC-coupled), [ADR-0064](0064-var-descriptor-carries-the-contained-value.md)
  (`.VAR` descriptors)
- Addresses: `todo/deep/native-method-cannot-return-an-lvalue-container.md`;
  `todo/tickets/lvalue-chain-through-at-key-at-pos-object-root.md` (closed by
  Slices 4 and 5, now
  `news/2026-09/lvalue-chain-through-at-key-at-pos-object-root.md`);
  `todo/tickets/rw-method-result-is-not-a-container-for-bind-or-invocant.md` and
  `todo/tickets/attribute-accessor-container-lost-in-argument-position.md`
  (closed by the returned-container-consumers slice, now
  `news/2026-09/rw-result-container-consumers.md`)

## Context

Two open findings say explicitly that they are the same problem and must be
designed together:

```raku
# (a) a method hands back its invocant's container
use v6.e.PREVIEW;
my $a = 42; $a.snitch = 666; say $a;   # raku: 42 then 666

# (b) an lvalue subscript chain that steps through an object with AT-KEY
class Q { has %.d is rw; method AT-KEY($k) is rw { %!d{$k} } }
class U { has Q $.query = Q.new(d => {foo => [1,2]}) }
my $u = U.new; $u.query<foo>[0] = 99; say $u.query.d;   # raku: {foo => [99 2]}
```

ADR-0059 established that **an `is rw` routine returns a container**, and built
two of the three halves that needs:

- **Production** — `compile_return_rw_arg` compiles a `return-rw` operand, and
  the bare tail of an `is rw`/`is raw` body, in container mode.
- **Consumption** — `assign_lvalue_container`
  (`src/runtime/lvalue_container_return.rs:33`) writes a value through a
  returned `Proxy` / `ContainerRef` / `HashEntryRef`.

What it did not build is the third half: **transport in the inbound direction**.
A routine can only hand back a location it was *given*, and mutsu's argument and
invocant paths hand it values.

This ADR closes that half, and shows that (a) and (b) are the same statement
applied at two different producers.

## Measured — what is actually broken (and what the findings got wrong)

Everything below was measured on 2026-09-05 against `raku` v2026.07 and a debug
`mutsu` built from `main` at `37dd63f33`. **Three of the two findings' central
claims are wrong**; they are corrected here rather than carried forward.

### Correction 1 — `.VAR` is not an acceptance case, and `.snitch` needs no parentheses

The deep file's headline repro claims `my $a = 42; $a.VAR = 5` prints `5` in
raku. It does not — raku dies with `Cannot assign to a readonly variable or a
value`. `$a.VAR` returns a **readonly `Scalar` object**, which is a different
feature from the raw-invocant return. (The file's own 2026-09-01 note already
caught this; the ticket text above it was never corrected, and the task that
produced this ADR was briefed with the wrong expectation.) The acceptance case
is `.snitch`, and it does *not* need the parenthesised spelling — plain
`$a.snitch = 5` works in raku too.

### Correction 2 — this is not a native-method problem

The deep file's root cause is "a *native* method has nowhere to put the answer".
That framing is wrong: the identical failure reproduces for a **user-written**
method, so nothing about it is native-specific.

| # | Program | raku | mutsu (before) |
|---|---|---|---|
| A3 | `my $a = 42; $a.snitch = 5; say $a` | `42` / `5` | `X::Assignment::RO: cannot assign through .snitch on non-instance` |
| A6 | `augment class Any { method mysn(\SELF:) is raw { SELF } }; $a.mysn = 5` | `5` | *the same error* |
| E1 | same, but **without** `is raw` on the method | `Cannot modify an immutable Int (42)` | the same error |
| E2 | `method mysn2(Any:D $s:) is raw { $s }` (non-raw invocant) | `Cannot assign to a readonly variable or a value` | the same error |

A6 vs E1/E2 pins the contract precisely: **both** the invocant parameter must be
raw (`\SELF:`) **and** the routine must be `is raw`/`is rw`. Neither alone.

### Correction 3 — the ticket's "at least loud and honest" claim covers one spelling out of four

The ticket says the (b) failure is now a loud refusal. That is true only for the
depth->=2 *method-rooted* spelling. Every neighbouring spelling is **silently
wrong**, and one is worse than silent:

| # | Program (with `class Q { has %.d; method AT-KEY($k) is rw { %!d{$k} } }`) | raku | mutsu (before) |
|---|---|---|---|
| B1 | `$u.query<foo>[0] = 99` (method-rooted, depth 2) | `{foo => [99 2]}` | loud refusal (the ticket's case) |
| C3/H1 | `my $q = Q.new(...); $q<foo>[0] = 99` (**variable**-rooted, depth 2) | `{foo => [99 2]}` | `{foo => [1 2]}` — **silent**, exit 0 |
| B4 | `$u.query<foo> = 99` (method-rooted, depth 1) | `{foo => 99}` | `{foo => 1}` — **silent** |
| B6 | `my $t := $u.query; $t<foo>[0] = 99` | `{foo => [99 2]}` | `{foo => [1 2]}` — **silent** |
| H2 | `$q.AT-KEY("foo")[0] = 99` (explicit spelling) | `{foo => [99 2]}` | `{foo => [1 2]}` — **silent** |
| H5/B8 | `$q<foo><bar>[0] = 99` (depth 3) | `{foo => {bar => [99 2]}}` | `No such method 'd' for invocant of type 'Hash'` — the **instance is replaced by a Hash** |
| C4 | `$q<foo> = 99` (var-rooted, depth 1) | `{foo => 99}` | `{foo => 99}` — correct |
| H3 | `my $e := $q<foo>; $e[0] = 99` | `{foo => [99 2]}` | `{foo => [99 2]}` — **correct** |
| H4 | `$q<foo>.push(99)` | `{foo => [1 2 99]}` | correct |

H3 is the load-bearing row: the `:=`-bind spelling of *exactly the same
subscript* already produces the right container. So (b) is not a missing
capability — it is a **producer that is not consulted**.

### The two real root causes

**(R1) The return side loses the container for a sigilless name.** Bytecode,
not inference:

```
sub f($x is rw) is rw { $x }     ->  GetLocal(0); WrapVarRef{name_idx:0,slot:0}; CaptureVarCell
sub f(\x)       is rw { x }      ->  GetLocal(0)
sub f(\x)       { return-rw x }  ->  GetLocal(0); CallFunc "return-rw"
```

Both routines store the parameter in the same slot under the same local name
`"x"`. The only difference is the **tail's AST node**: `$x` parses to
`Expr::Var("x")`, a sigilless `\x` parses to `Expr::BareWord("x")`, and
`scalar_container_alias_name` (`src/compiler/expr_call.rs:41`) matches only
`Expr::Var`. So `return_rw_container_name` never fires and no cell is captured.
This is the entire cause of the `sub f(\x) is raw { x }; f($a) = 5` family.

**(R2) The invocant never arrives as a container at all.** Positional raw
parameters *do* (`sub f(\x) { x = 7 }` writes the caller's `$a`; so does
`f(@a[0])`), but the invocant does not:

| # | Program | raku | mutsu |
|---|---|---|---|
| F3 | `sub f(\x) { x = 7 }; my $a = 42; f($a); say $a` | `7` | `7` |
| G3 | `sub f(\x) { x = 9 }; my @a = 1,2; f(@a[0]); say @a` | `[9 2]` | `[9 2]` |
| I1 | `augment class Any { method mut(\S:) { S = 7 } }; my $a = 42; $a.mut; say $a` | `7` | **`42`** |
| I3 | the same over `@a[0].mut` | `[7 2]` | **`[1 2]`** |

**Methodological note, because it nearly produced a wrong ADR.** The obvious
probe — `method vv(\S:) { say S.VAR.WHAT }` — reports `(Scalar)` in mutsu, which
looks like the container arrived. It is a **false positive**: ADR-0064 makes
`.VAR` synthesise a descriptor from the contained value, so it answers `Scalar`
whether or not a real cell exists. Only the *mutation* discriminator (I1/I3)
separates the two. Do not use `.VAR` as a container oracle here.

### What `.item` is, and why it is not the design

`$a.item = 5` already works in mutsu, and `scalar_container_alias_name`'s doc
comment presents it as "a native raw-invocant method handing the container
back". The bytecode says otherwise:

```
my $a = 42; $a.item = 5   ->  LoadConst(5); AssignExprLocal(0); TagContainerRef(0, Some(0))
my $a = 42; $a.snitch = 5 ->  ... CallFunc "__mutsu_assign_method_lvalue" (arity 5)
```

`.item` is **erased at compile time** — the call disappears and the assignment
becomes a plain store to `$a`. That is sound for `.item` only because `.item` is
pure. It cannot be generalised: `.snitch` **notes its invocant** (raku prints
`42` before `666`), and erasing the call would drop the side effect. So the
cheap route — "add `snitch` to the erasure list" — is not merely a band-aid, it
is *incorrect*. This ADR takes the real route instead.

### Where the object-rooted chain loses the write (R3, a consequence of R2)

`exec_index_assign_expr_nested_op` (`src/vm/vm_var_assign_index_named.rs:2963`)
*does* have an Instance branch. Under `rust-gdb`, breaking at :2984, :3014 and
:3033 on the H1 repro shows it entered, called `AT-KEY`, and then fell straight
through to the generic Hash/Array walk:

```
Breakpoint 1 (:2984  "if let Some(at) = at")      hit
Breakpoint 2 (:3014  "let idx_u = ...")           hit
Breakpoint 3 (:3033  "let inner_key = ...")       hit   <- generic walk, root is the Q Instance
```

The branch calls the accessor as an **rvalue** and discards its container on the
next line:

```rust
let inner = self
    .call_method_with_values(target, at, vec![inner_idx.clone()])?
    .deref_container();                       // <- the container is thrown away here
```

It then writes only if the element happens to be a `Proxy`, or the inner value
is itself an Instance with `ASSIGN-POS`/`ASSIGN-KEY`. For the ordinary case it
falls out and the generic walk runs against a root that is not a container — the
write is dropped (and at depth 3 the root is replaced by a fresh Hash, which is
where H5's nonsense error comes from).

## Decision

**A raw parameter binds the caller's container, and the invocant is parameter
zero. A routine declared `is rw`/`is raw` (or spelling `return-rw`) hands that
container back, and every lvalue consumer writes through it — including the
subscript-chain walker, which takes its step through an object by calling
`AT-KEY`/`AT-POS` in that same lvalue mode.**

One rule, four mechanical parts. Three of the four already exist and are simply
not connected; none of this is new machinery.

### 1. Rw-capability is one declaration oracle

`Interpreter::routine_is_rw_capable` (`src/runtime/builtins_lvalue.rs:251`)
already states the rule — `is rw || is raw || body spells return-rw` — and the
**sub** lvalue path uses it. The **method** path does not: it tests
`method_def.is_rw` alone (`methods_mut_method_lvalue.rs:1363`), and `MethodDef`
has no `is_raw` field at all because `Stmt::MethodDecl` never carried one
(`src/ast.rs:1348`; `SubDecl` carries both, `src/ast.rs:1026`). Measured
consequence:

| # | Program | raku | mutsu |
|---|---|---|---|
| K1 | `class C { method m(\x) is rw { x } }; C.new.m($a) = 5` | `5` | `5` (after Slice 1) |
| K2 | the same with `is raw` | `5` | `X::Assignment::RO: method 'm' is not rw` |
| K4 | `method m(\x) { return-rw x }` | `5` | the same error |

(K1/K2/K4 re-verified against raku v2026.07 on 2026-09-05 before Slice 2 was
written; all three now answer `5` in mutsu.)

So: plumb `is_raw` onto `MethodDecl`/`MethodDef` and route the method gate
through `routine_is_rw_capable`. One oracle, two callers — not two rules.

For **native** methods, invocant-rawness is likewise a declaration, and it needs
a place to live: a single table of native methods that are `is raw` on their
invocant, transcribed from Rakudo's signatures (`.snitch`, `.item`, and the
container-carrying `.list`). This is deliberately a *declaration table*, not a
call-site name check — the difference is that every consumer (the compiler's
argument mode, the runtime dispatch, the lvalue gate) reads the same row, so the
family cannot drift apart the way `.item`'s compiler-only erasure did.

### 2. Inbound transport — the invocant is compiled in container mode

The container-mode argument compile already exists
(`compile_rw_chain_index_arg`, `compile_return_rw_arg`) and is already applied
to the arguments of a call nested inside a `return-rw` operand. Apply the same
mode to the **invocant** of a call to a raw-invocant routine.

The call site is already 90% there. `$a.snitch = 5` compiles to

```
GetLocal(0); ContainerizePair; WrapVarRef{name_idx:0, slot:0}; ... CallFunc "__mutsu_assign_method_lvalue"
```

— the invocant is *already* tagged with `WrapVarRef`, carrying its source name.
The missing op is the one the `return-rw` tail emits right after it:
`OpCode::CaptureVarCell`, which boxes the named slot into the shared cell. With
the invocant compiled in container mode, the element and attribute spellings
raku supports come for free, because container mode is what already handles
them:

| # | Program | raku |
|---|---|---|
| E4 | `my @a = 1,2; @a[0].snitch = 9; say @a` | `[9 2]` |
| E5 | `my %h = a=>1; %h<a>.snitch = 9; say %h` | `{a => 9}` |
| E6 | `class C { has $.v is rw }; $c.v.snitch = 9` | `9` |

That is the whole reason to do this in the shared container-mode compile rather
than at the `$a`-shaped call site: there is no per-shape code.

### 3. Outbound transport — a sigilless name denotes its container

`Expr::BareWord` is how the parser spells a sigilless lexical, *and* how it
spells a type name, an enum value and a bare call. It denotes a container
exactly when it resolves to a local slot of the frame being compiled — which is
what `local_map` records. So the rw-tail site consults `local_map` rather than
guessing from the spelling.

This is scoped to `return_rw_container_name` and deliberately **not** folded
into `scalar_container_alias_name`, whose other three callers (List-literal
elements `src/compiler/expr.rs:330`, fat-arrow Pair values
`src/compiler/expr_binary.rs:707`) legitimately see barewords that are type
names in ordinary code — `my @a = (Int, Str)` must not box `Int`.

### 4. Consumption — unchanged, and that is the point

`assign_lvalue_container` already writes through `Proxy` / `ContainerRef` /
`HashEntryRef`. Nothing in this ADR adds a consumer; parts 1-3 exist so that the
one consumer ADR-0059 built is actually reached.

### 5. (b) is part 2 applied to one more producer

The chain walk's step through an object becomes: *call `AT-KEY`/`AT-POS` in
lvalue mode and descend into the container it returns*, replacing
`call_method_with_values(...).deref_container()`. Because the accessor is `is
rw`, `routine_is_rw_capable` says yes, container mode produces a `ContainerRef`
(or a `HashEntryRef` for a missing key), and the walk already knows how to
descend into a `ContainerRef` — `descend_container_ref`
(`vm_var_assign_index_named.rs:3401`) and `env_root_descended_mut` (:3434) do
exactly that. H3 proves the produced container is the right one.

The same substitution serves the *method-rooted* spelling: the compiler temp
`bind_method_rooted_chain_root` installs (`src/compiler/expr_closure.rs:606`) is
filled by a plain `self.compile_expr(cur)` — an rvalue read. Compiling that root
in container mode makes the temp hold the accessor's container, and
`lvalue_root_temp_not_a_container`'s loud refusal (:3470) then fires only for
roots that genuinely are not locations.

**Explicitly rejected:** reintroducing an accessor-keyed slow path. The deleted
`__mutsu_index_assign_method_lvalue_nested` is what silently dropped the writes
this ticket's neighbours were about
(`news/2026-09/method-rooted-lvalue-subscript-chain-writes-through.md`), and its
copy-on-write model is the specific thing that made autovivified levels
evaporate. The routing above adds no new walker at all.

## Which dispatch paths must preserve a container invocant

Enumerated, not hand-waved. A container invocant must survive from the call site
to the routine body on each of these, and each currently derefs or is simply not
reached:

| Path | Site | Status |
|---|---|---|
| `OpCode::CallMethod` | `src/opcode.rs:1262` | ordinary rvalue call — must stay value-passing except for a raw-invocant callee |
| `OpCode::CallMethodMut` | `src/opcode.rs:1285` | lexical receiver; already retains a `ContainerView` cell for non-`WHAT`/`VAR` methods (`vm_call_method_mut_ops.rs:682`) — the nearest thing to a working precedent |
| `OpCode::CallMethodDynamic` / `…DynamicMut` | `src/opcode.rs:1298`/`:1308` | computed method name; rawness is only knowable at runtime, so the *runtime* gate (part 1's oracle) has to decide |
| `OpCode::HyperMethodCall` / `…Dynamic` | `src/opcode.rs:2041`/`:2056` | `>>.` — raku does not give a hyper call an lvalue result; must keep decontainerizing, and this is a deliberate non-goal |
| `__mutsu_assign_method_lvalue` -> `assign_method_lvalue_with_values` | `methods_mut_method_lvalue.rs:146` | the lvalue entry; already receives a `WrapVarRef`-tagged invocant, missing `CaptureVarCell` |
| `try_rw_method_container_lvalue` | `lvalue_container_return.rs:125` | type-object invocant half; unaffected (a type object has no container) |
| `call_method_with_values` | `methods_call_dispatch.rs:140` | takes `target: Value`, so it can already carry a `ContainerRef`; the derefs are downstream |
| `native_method_0arg` / `_1arg` / `_2arg` | `builtins/methods_0arg/mod.rs:304`, `methods_narg/dispatch_1arg.rs:25`, `dispatch_2arg.rs:17` | take `target: &Value` — a native method can already *receive* a container; the raw-invocant table decides which ones must return it unchanged |

## Slices

Each slice is independently verifiable and independently shippable.

### Slice 1 — a sigilless name denotes its container (IMPLEMENTED 2026-09-05)

Part 3 above. `return_rw_container_name` becomes a method so it can consult
`self.local_map`, and gains a `BareWord` arm gated on
`is_plain_lexical_name(name) && self.local_map.contains_key(name)`.

**Acceptance** (all verified identical under raku v2026.07):

```raku
sub f(\x) is raw { x }; my $a = 42; f($a) = 5;              # 5
sub f(\x) is rw  { x }; my $a = 42; f($a) = 5;              # 5
sub f(\x) { return-rw x }; my $a = 42; f($a) = 5;           # 5
sub f(\x) is raw { x }; say f($a).VAR.^name;                # Scalar
class C { method m(\x) is rw { x } }; C.new.m($a) = 5;      # 5
sub f(\x) is raw { x }; my @a = 1,2; f(@a[0]) = 9;          # [9 2]
```

Pinned by `t/sigilless-raw-param-container-return.t` (21 tests, byte-identical
output under `mutsu` and `raku`), which includes the three non-regression rows
that constrain the gate: `my @a = (Int, Str)`, `my $p = (a => Int)`, and
`sub f() is raw { my \w = 5; w }`.

### Slice 2 — one rw-capability oracle for methods (IMPLEMENTED 2026-09-05)

Part 1's user half: add `is_raw` to `Stmt::MethodDecl` and `MethodDef` (~18
construction sites), parse the trait, and replace
`methods_mut_method_lvalue.rs:1363`'s `!method_def.is_rw` with
`routine_is_rw_capable`. Mechanical and wide; no design left in it.

**Acceptance:** K2 and K4 above.

**What actually shipped.** `is_raw` is now carried on `Stmt::MethodDecl`
(`src/ast.rs`), `CompiledMethodDecl` (`src/opcode.rs`) and `MethodDef`
(`src/runtime/decl_types.rs`); the parser already produced `traits.is_raw` and
only had to stop discarding it. The oracle is
`Interpreter::method_is_rw_capable` (`src/runtime/builtins_lvalue.rs`) —
`is_rw || is_raw || body_uses_return_rw(body)`, the `MethodDef`-shaped twin of
`routine_is_rw_capable` — and it now backs all three method gates:
`methods_mut_method_lvalue.rs`'s unqualified and `Class::method` refusals, and
`method_lvalue_returns_container` (`lvalue_container_return.rs`), which is what
also blocks the legacy setter convention from pre-empting the lvalue return.

**One thing the slice description got wrong, measured.** Routing the *runtime*
gate through the oracle is necessary but not sufficient: with only that change,
K4 (`return-rw`) passed and K2 (`is raw`) still failed with
`X::Assignment::RO: rw method 'm' does not expose an assignable attribute` —
the gate now admitted the call, but the method body's tail had been compiled as
a plain value read, so there was no container to write through. The **compile**
side keys the rw tail off the same declaration and had the identical `is_rw`-only
narrowness, in two places: `decl_plan.rs`'s main-pass `compile_method_body` call
and `accessors_resolve.rs`'s registration-time
`compile_method_def_in_place_with_dist`. Both now pass `is_rw || is_raw`,
mirroring `compile_sub_body`'s long-standing `sub_compiler.rw_tail = is_rw ||
is_raw`. The lesson generalises to slices 3-5: *a capability that is gated at
runtime is usually also gated at compile time, and the two must move together.*

Beyond K2/K4 this also fixed the **type-object** invocant twins, which were
worse than a refusal: `class C { method m(\x) is raw { x } }; C.m($a) = 5`
silently reported success and dropped the write (measured `42` where raku says
`5`), because `try_rw_method_container_lvalue` declined and the legacy setter
convention swallowed the assignment.

Pinned by `t/method-rw-capability-oracle.t` (22 tests, byte-identical output
under `mutsu` and `raku`): the three rw-capable spellings over instance and
type-object invocants, over scalar / array-element / hash-element containers,
through `multi`, role composition and `augment`; plus the non-rw-capable
regression controls and the `is rw` attribute-accessor shapes the oracle must
not have disturbed.

**Adjacent divergence found and deliberately not fixed here.** For a
**type-object** invocant whose method is *not* rw-capable, mutsu still reports
success and drops the write (raku dies), because the legacy
`$obj.name($value)` setter convention catches it — and for a sigilless
parameter it calls the method with the *invocant* as its argument. The
instance twin already refuses correctly. It is a different mechanism with no
declaration-level oracle to gate on, and retiring or gating it needs its own
corpus measurement, so it is recorded as
`todo/tickets/type-object-lvalue-falls-into-setter-convention.md` rather than
folded in.

### Slice 3 — the invocant arrives as a container

Parts 1 (native table) and 2. Emit `CaptureVarCell` for the invocant of a call
to a raw-invocant routine at the lvalue call site, teach the runtime dispatch
not to deref it, and have the raw-invocant natives return it unchanged.

**Acceptance:** A3/A6 (`$a.snitch = 5` -> `42` then `5`; the `augment` twin),
I1/I3 (mutation *through* a raw invocant reaches the caller), and E4/E5/E6 (the
element and attribute invocant spellings). E1/E2 must keep refusing.

#### Re-scoped 2026-09-05, after Slice 2 landed: this is two slices, not one

Every row above was re-measured against raku v2026.07 and a debug `mutsu` built
from `main` at `ec80a6c82` + Slice 2. All of the ADR's original numbers still
hold — Slice 2 moved none of them, and `E1`/`E2` still refuse. What the
re-measurement *did* change is the estimate of where the work is. **"Emit
`CaptureVarCell` at the lvalue call site" is not one edit, because the invocant
is not an ordinary argument in mutsu**, and the two acceptance families reach
the invocant through entirely disjoint machinery:

**3a — the lvalue half (A3/A6/E4/E5/E6), `$a.snitch = 5`.** The call site is
`__mutsu_assign_method_lvalue`, and the invocant *is* already tagged:
`GetLocal(0); ContainerizePair; WrapVarRef{name_idx:0, slot:0}` (confirmed with
`--dump-bytecode`). The missing box is `CaptureVarCell`, exactly as this ADR
said — but it cannot be emitted unconditionally at that site. Rawness is not
statically known (`$a.snitch`'s callee depends on `$a`'s runtime type and on a
runtime method-name string), and boxing every lvalue invocant would hand a
`ContainerRef` to the ~40 `target.view()` branches of
`assign_method_lvalue_with_values` that today match `Instance`/`Array`/`Hash`
directly, silently skipping all of them. It also cannot be boxed inside that
function: `capture_var_cell_inner` needs the frame's `&CompiledCode` for its
slot resolution, which the runtime entry does not have. The viable shape is a
**runtime-gated box in the VM**, where both the frame's `code` and the resolved
callee are in hand — around `dispatch_func_call_inner`'s
`__mutsu_assign_method_lvalue` arm — plus the native raw-invocant declaration
table (`.snitch`, `.item`, `.list`) part 1 calls for, plus making
`dispatch_snitch` log `deref_container()` while returning the container it was
handed.

The **element and attribute spellings do fall out**, as the ADR predicted, but
for a different reason than "container mode handles them": `@a[0].snitch = 9`
already compiles to a copy-in/copy-out protocol
(`SetGlobal(tmp); …lvalue call…; GetGlobal(tmp); IndexAssignExprNamed`, see
`expr_call.rs`'s `__mutsu_assign_method_lvalue` + `Expr::Index` arm), so a write
through the *temp's* container is written back into `@a[0]` by the existing
tail. E4/E5/E6 therefore need no per-shape code — they need the temp to be the
thing that gets boxed.

**3b — the arrival half (I1/I3), `$a.mut` where `method mut(\S:) { S = 7 }`.**
This shares no code with 3a. It is an ordinary `CallMethodMut`, and the invocant
travels as a bare `target: Value` from the opcode to the binder, losing every
trace of where it came from. Measured under `rust-gdb` on I1
(`break vm_method_dispatch.rs:1512`):

```
call_compiled_method_fast(receiver_class_name="Int", method_name="mut", base=…)
  <- call_compiled_method                (vm_method_dispatch.rs:298)
  <- dispatch_compiled_method            (vm_call_method_compiled_cache.rs:393)
  <- try_dispatch_compiled_method_direct_as (vm_call_method_compiled_direct.rs:98)
param_name = "S"    ->    param_values.push((param_name, base.clone()))
```

So the invocant parameter is bound to the invocant **value** at
`vm_method_dispatch.rs:1512`, and there is a **second, independent** binding of
the same parameter in the slow binder at `:581`
(`env_mut().insert(param_name, base.clone())`) — which of the two runs is
decided by the `fast_method_cache` eligibility gate
(`vm_call_method_compiled_cache.rs`). Both would have to learn the container,
and something upstream would have to produce one: `CallMethodMut` does carry
`target_name_idx` (the source name `"a"` is in the opcode), but neither
`dispatch_compiled_method` nor `call_compiled_method` takes an argument-source
channel today, so 3b is a signature change across that whole chain plus a new
gate in a hot dispatch path. That is a materially different, higher-blast-radius
change than 3a, and pairing them in one PR would make a red CI unattributable.

**Decision: split.** 3a and 3b ship separately, 3a first (it is the half the two
originating findings actually asked for, and its acceptance rows A3/A6/E4/E5/E6
are self-contained). I1/I3 move to 3b. E1/E2 stay regression controls for both:
raku needs the invocant raw **and** the routine `is raw`/`is rw`, and dropping
either must keep refusing.

One more thing 3a has to settle that the original text did not anticipate:
`capture_var_cell_inner` boxes a **frame local**, and returns the value
unchanged when the name is not one (`vm_data_ops.rs`'s `let Some(idx) = idx
else { … return inner }`). `$a.snitch` boxes a local fine, but the E4/E5
spellings hand the lvalue call a *global temp*
(`__mutsu_tmp_assign_method_target_N`), which that helper cannot box. So 3a
needs either a global-name container route or the temps promoted to locals —
a choice worth making explicitly rather than discovering mid-slice.

#### Slice 3a — IMPLEMENTED 2026-09-05

Every row below was re-measured against raku v2026.07 and a debug `mutsu` built
from `main` at `f833d9893` before any code was written; all of the ADR's numbers
still held.

**What shipped.**

- **The declaration oracle** (`src/runtime/raw_invocant.rs`, new).
  `Interpreter::method_returns_raw_invocant(target, method, args)` answers the
  ADR's contract as one function: resolve the routine (a user method always
  wins over the native table, as ordinary dispatch does) and require **both**
  `method_is_rw_capable` (slice 2's oracle) **and** a raw invocant parameter.
  Raw-invocant spellings, all verified against raku: the sigil-less `\S:` (the
  parser records `sigilless: true, is_invocant: true`), `$s is raw:` and
  `$s is rw:`. `Any:D $s:` is not raw — the E2 control. The invocant class is
  resolved with `what_type_name` rather than ADR-0059's Instance/type-object-only
  helper, because a raw invocant is precisely the case where the invocant is an
  ordinary `Int` and the routine came from `augment class Any`.
- **The native declaration table**, in the same module. It has exactly one row,
  `snitch`, gated on 6.e (below which the method does not exist at all). The
  ADR's other two proposals were measured and **do not belong**: `$a.list =:= $a`
  is `False` and `.list` returns a `List`, so `$a.list = 7`'s reaching `$a` is
  *list assignment* into a List whose element is the invocant's container — a
  different mechanism, and listing it here would silently replace it. `.item` is
  genuinely raw (`$a.item =:= $a` is `True`) but the compiler erases
  `$a.item = 5` to a plain store, so the row would never be consulted.
- **The runtime-gated box** (`src/vm/vm_raw_invocant_lvalue.rs`, new), called
  from `exec_call_func_op`'s `__mutsu_assign_method_lvalue` arm — the only site
  where the frame's `code` (needed for slot resolution) and the invocant value
  are both in hand.
- **The consumer**, `try_raw_invocant_container_lvalue`, spliced into
  `assign_method_lvalue_with_values` immediately after the type-object half. It
  runs the routine with the container invocant and writes through whatever
  container comes back — the general rule, so `method m(\S:) is raw { 42 }` is
  refused exactly as raku refuses it.
- **`dispatch_snitch`** now logs `deref_container()` and returns the invocant
  exactly as given, container and all.

**The global-temp decision: a global-name container route, not local promotion.**
`capture_lvalue_invocant_cell` tries four routes in order — (1) `capture_var_cell`
for a frame local, (2) an existing container already sitting in **env** under
that name, (3) a direct slot box for a `$`-scalar local whose value is
*reference*-shaped, (4) a freshly minted cell stored in **env under the name**.
Route 4 is what serves E4/E5. Promoting the temps to locals was rejected on
blast radius: `__mutsu_tmp_assign_method_target_N` is read back by the copy-out
tail through `GetGlobal` and by `IndexAssignExprNamed`, so promoting it would
touch the whole temp protocol for every lvalue method call, whereas the env cell
is transparent — `GetGlobal` already dereferences a `ContainerRef`, so the tail
reads the written value and `IndexAssignExprNamed` puts it back into `@a[0]` /
`%h<a>` with no per-shape code, exactly as this ADR predicted. Route 4 is
restricted to scalar-shaped values (mirroring `capture_var_cell_inner`'s own
`is_reference` guard) so an `Array`/`Hash` env entry is never given a cell that
disagrees with its identity-shared storage.

**Route 2 exists because the first ordering shipped a silent wrong answer, and
the rule it encodes generalises: reusing an existing location must always come
before minting one.** With routes 1/3/4 only, `for @a -> $e is rw
{ $e.m = 3 }` left `@a` untouched (raku: `[3 3]`) — where before the slice it
had refused loudly. The loop parameter binds the *element's own promoted cell*
(`vm_for_loop_body.rs`'s `aliased` path, which then suppresses the end-of-
iteration writeback precisely because the alias carries the write), and that
cell lives in env rather than in a frame slot, so the env route minted a second,
disconnected cell over the top of it. Route 2 is the env-side twin of the check
`capture_var_cell_inner` already applies to a local slot
(`is_lvalue_container_value`). It is pinned by the `is rw` and `<->` loop-
parameter rows, so the ordering cannot silently regress.

**Route 3 was added after measurement, and closes a silent wrong answer.**
`class C { method m(\S:) is raw { S } }; my $c = C.new; $c.m = 5` is `5` in raku
(the raw invocant is the *variable's* container, so the write replaces its whole
contents); mutsu reported success and dropped the write. `capture_var_cell_inner`
deliberately refuses to re-containerize a reference for the general capture
paths, so this route boxes the slot directly — narrowly, only for a `$`-scalar
local (`@a`/`%h` locals keep their sigil in `code.locals`) and only behind the
raw-invocant gate.

**Slice 2's lesson did not repeat, and the ADR's reason why is worth recording.**
The runtime gate was sufficient here without a matching compile-side change,
because the routine's *body* was already compiled correctly — slice 2 had
already widened `compile_method_body`'s rw-tail flag to `is_rw || is_raw`, and
slice 1 had already made the sigil-less `BareWord` tail (`{ S }`) denote its
container. 3a only had to make the *invocant* arrive as one. The three slices
compose exactly as the ADR's "one rule, four mechanical parts" claimed.

**A cost that was measured, and paid down.** The VM gate runs on every
`__mutsu_assign_method_lvalue` call — i.e. on every `$obj.attr = v` — and the
first version cost **+13%** on a tight `$p.x = $i` loop (same-binary env-switch
A/B on a release build, the only reliable way to compare: median 1.92s with the
gate against 1.70s with it skipped). So the slice carries a pre-filter:
`Registry::any_raw_invocant_method`, a **set-only** flag raised at registration
by `note_raw_invocant_methods` whenever a `MethodDef` with a raw invocant enters
`user_candidates`. Set-only is the safe direction — a stale `true` costs only
the resolve that would have happened anyway, while a spurious `false` would
silently switch the feature off.

Two things about it are load-bearing. First, **it sits in the VM gate, ahead of
every allocation, not inside the oracle.** Placing it inside
`method_returns_raw_invocant` recovered almost nothing (~1.6%), which located
the real cost: most of the 13% was the *argument extraction* the gate does
before it can even ask — two `to_string_value()` allocations and a
`method_args` vector — not `resolve_method` at all. The shipped gate asks the
flag against a **borrowed** method name (`Value::as_str`) and returns before
allocating anything. Second, the filter and the oracle read the **same**
`method_def_has_raw_invocant` predicate, so they cannot disagree by
construction; a `debug_assert` re-derives the slow answer whenever the filter
declines, turning any future registration path that bypasses `Registry`'s
mutators into a deterministic failure of the debug `t/` suite rather than a
feature that silently stops working.

With the filter, the min-of-14 under load is 2.38s against 2.65s for the
un-filtered gate — the regression is recovered.

**One guard the boxing required.** Every path below the new branch in
`assign_method_lvalue_with_values` matches `Instance`/`Array`/`Hash` directly and
would silently skip a `ContainerRef`, so the target is decontainerized at a
single chokepoint right after the branch declines. That is what keeps the boxing
invisible to the other ~40 branches — the specific hazard this slice's re-scoping
identified.

**Pinned by** `t/raw-invocant-lvalue-container.t` (29 tests) and
`t/snitch-lvalue-raw-invocant.t` (12 tests), both byte-identical under `mutsu`
and `raku`. Between them they cover the three rw-capable spellings, both sigiled
raw-invocant spellings, the array-element / hash-element invocants, the
instance-valued scalar, the observing body, the unchanged rvalue call
(`$a.snitch =:= $a`), a `Str` and an uninitialized (type-object) invocant, the
runtime method-name spelling, a `multi` candidate selected by a real argument,
each frame shape the four routes serve (a sub's own local, a captured-outer
scalar written from a closure, an `is rw` loop parameter and its `<->` twin),
and the three regression controls: not rw-capable, not a raw invocant, and a
raw-invocant routine that returns a value rather than a location.

**E6 does not belong to 3a — measured, and it is not reachable from this
mechanism.** `class C { has $.v is rw }; $c.v.snitch = 9` compiles with **no
temp and no writeback tail**: the invocant is read by a bare `CallMethodMut` on
the accessor and argument 4 is `LoadNil`, so there is no name to box and nothing
would read a cell back. The producer it needs already exists — `MarkAccessorRefContext`,
which is what makes `my $x := $c.v; $x = 9` write through today — but emitting it
before an lvalue invocant is an *unconditional compile-side* change (rawness is
not statically known), so it must be paired with the decontainerize-at-the-chokepoint
guard above and re-measured across every `$obj.acc.m = v` shape. That is its own
slice. The mutation discriminator, not `.VAR`, is what settled this: `$c.v`
produces a container for a `:=` bind but not in argument position, where
**mutsu** died with "expects a writable container" (raku answers `9`).

> **Correction (2026-09-06, from the E6 producer's own re-measurement).** The
> sentence above originally read "(`sub g($y is rw) {...}; g($c.v)` dies with
> 'expects a writable container')" as a statement about **raku**. It is mutsu's
> diagnostic; raku answers `9`. The text is corrected in place above, and the
> conclusion the row was cited for — that E6 is a producer question and `.VAR`
> is not the discriminator — is unaffected. But argument position was a *third*
> consumer that was still broken, and its `is raw` twin
> (`sub f(\x) is raw { x }; f($c.v) = 9`) was **silently wrong** (`42` where
> raku says `9`), not merely a copy as the non-goals section said. Both are
> closed by the returned-container-consumers slice below, which re-verified the
> misattribution against raku v2026.07 before touching any code.

**Also still refusing after 3a, all loudly (not silently wrong), all out of
scope:** `@a.snitch = (7,8)` and `%h.snitch = (b=>2)` (aggregate invocants —
route 4's scalar restriction declines them), `$a.list = 5` (list assignment, see
above), `$a.snitch.snitch = 5` (a chained lvalue invocant),
`@n[0][1].mutsuRawInv = 8` (a depth-2 subscript invocant, which is slice 4's
walker), and `42.snitch = 5` (raku also dies, with a different message).

#### Slice 3b — IMPLEMENTED 2026-09-06

Every row of the I/E/F/G families above was re-measured against raku v2026.07
and a debug `mutsu` built from `main` at `30d6754f5` before any code was
written, along with a new L/M/N/O/Q/R/S family built to find the *edges* of the
contract. Two of this ADR's own claims about 3b did not survive that.

**Correction A — `is raw` on the routine is not part of the arrival contract,
and E1/E2 are not regression controls for this half.** The slice 3 text says
"E1/E2 stay regression controls for both: raku needs the invocant raw **and**
the routine `is raw`/`is rw`, and dropping either must keep refusing". Measured,
that conjunction is the *outbound* (lvalue-return) contract only:

| # | Program | raku |
|---|---|---|
| L1 | `class C { method m(\S:) { S = 7 } }; my $c = C.new; $c.m; say $c` | `7` |
| L2 | the same with `method m($s is raw:)` | `7` |
| L3 | the same with `method m($s is rw:)` | `7` |
| L4 | the same with `method m($s:)` | dies, `Cannot assign to a readonly variable or a value` |
| L5 | the same with `method m(C $s:)` | dies, `Cannot assign to an immutable value` |

No `is raw` on the routine anywhere in L1-L3. `is raw`/`is rw` on the *routine*
answers "is this call an lvalue"; rawness of the *invocant parameter* answers
"does the body's write reach the caller". They are independent, and 3b needs
only the second. So the shipped oracle is
`Interpreter::method_binds_raw_invocant` — the same resolve as slice 3a's
`method_returns_raw_invocant` with the `method_is_rw_capable` conjunct dropped.
Both read the one `method_def_has_raw_invocant` predicate, so the two halves
cannot disagree about what "raw invocant" means. E1/E2 remain slice 3a's
controls and are re-verified unchanged; L4/L5 are 3b's own controls.

**Correction B — the recorded call chain was the wrong one, and the transport
the ADR predicted is not the transport that shipped.** The ADR's `rust-gdb`
trace has `call_compiled_method_fast <- call_compiled_method <-
dispatch_compiled_method <- try_dispatch_compiled_method_direct_as`, and
concludes 3b "is a signature change across that whole chain". Re-traced on I1
and L2, the chain above `call_compiled_method` is **two** different chains and
neither is the recorded one at its top:

```
$c.m, Instance receiver:
  exec_call_method_mut_op_impl (vm_call_method_mut_ops.rs)   target_name = "c"
    -> try_compiled_method_mut_or_interpret_sym              target_name = "c"
      -> call_compiled_method            (:581,  slow binder, `$s is raw:`)
        -> call_compiled_method_fast     (:1512, fast binder, `\S:`)

$a.mut, `augment class Int` receiver:
  exec_call_method_mut_op_impl                               target_name = "a"
    -> try_compiled_method_mut_or_interpret_sym
      -> vm_call_method_mut_with_values -> call_method_mut_with_values
        -> call_method_with_values -> call_method_with_values_inner
          -> try_dispatch_compiled_method_direct_as -> dispatch_compiled_method
            -> call_compiled_method -> call_compiled_method_fast
```

The second chain runs through `call_method_with_values`, which takes
`target: Value` and no source channel and is called from ~everywhere, so the
signature change the ADR imagined would have been far wider than "that whole
chain". What both chains *do* share is their single origin, the `CallMethodMut`
opcode, which is the only place that has the receiver's source name at all.
So the shipped transport is a **one-slot channel armed at that opcode and
consumed at the binder**, not a parameter: `pending_raw_invocant`
(`src/vm/vm_raw_invocant_arrival.rs`), armed immediately before the dispatch,
disarmed immediately after, and consumed only by a binder that both agrees on
the method name and is looking at a `ParamDef` that really is a raw invocant.
That last re-check is what makes a nested dispatch in the window (a `where`
clause, a multi tie-break) unable to mis-bind it — and it is also the
authority when multi-dispatch lands on a different candidate than the gate
resolved.

The ADR *was* right that both binders must learn the container, and right about
which is which: `binds_caller_container()` is true for `$s is raw:` / `$s is rw:`
(the traits arm), which routes those to the slow binder, while a sigil-less
`\S:` is excluded by that predicate's own `!pd.is_invocant` and lands on the
fast one. The shipped pin exercises both spellings for exactly this reason.

**What shipped.**

- **`Interpreter::method_binds_raw_invocant`** (`src/runtime/raw_invocant.rs`),
  the arrival oracle described above. No native fallback row: no native method
  mutates its invocant through parameter zero (`.snitch`, slice 3a's one row,
  only hands it back).
- **The arrival channel and its arm/disarm pair**
  (`src/vm/vm_raw_invocant_arrival.rs`), wired into `CallMethodMut`'s two
  user-method dispatch sites through one helper so the pair can never be split,
  and into `CallMethodDynamicMut` for the runtime method-name spelling.
- **No new producer.** The container comes from slice 3a's
  `capture_lvalue_invocant_cell`, reused verbatim — which is the point: its
  route order (an existing frame cell, then an existing env container, then a
  direct slot box, then a minted cell) is the thing slice 3a had to learn the
  hard way, and reusing it is what makes `for @a <-> $e { $e.m }` and
  `for @a -> $e is rw { $e.m }` bind the element's *already promoted* cell
  instead of a second, disconnected one. Both are pinned.
- **The binder change is two lines in each binder**: bind parameter zero to the
  channel's cell when it holds one, and leave `base` alone. `base` stays the
  plain value, so `self`, the attribute seeding, the dispatch frame and the
  ~40 downstream `target.view()` branches see exactly what they see today —
  the 3a hazard ("boxing every invocant would hand a `ContainerRef` to branches
  that match `Instance`/`Array`/`Hash` directly") does not arise here at all,
  because only the *parameter* is boxed, never the invocant value.
- **Write-through needs no new consumer either.** A local slot holding a
  `ContainerRef` already stores through the cell (`vm_var_assign_set_local.rs`,
  `vm_var_assign_local.rs`) and `GetLocal` already derefs one, so `S = 7` inside
  the body reaches the caller with no writeback machinery. Slice 3b adds no
  writeback path, no `pending_rw_writeback_sources` entry, and no new opcode.

**The cost, measured.** The gate runs on **every** `$var.method(...)`, an
order of magnitude more traffic than slice 3a's `$obj.attr = v`. Slice 3a's
pre-filter is `self.registry().any_raw_invocant_method`, which is an `RwLock`
read acquisition — affordable there, not here. So the flag is mirrored into a
process-global, set-only `AtomicBool` raised by the *same* writer
(`Registry::note_raw_invocant_methods`), the pattern `env.rs`'s
`CLOSURE_META_KEY_SEEN` already uses, and a `debug_assert` in
`debug_verify_owner_method_names_index` fails the debug `t/` suite if a future
writer of the registry field ever bypasses that one writer. With the mirror,
the whole gate for a program that declares no raw-invocant method is one relaxed
atomic load and an `is_empty()`.

Same-binary env-switch A/B on a release build (`MUTSU_SKIP_RAW_INVOCANT_ARRIVAL`
short-circuits the gate), 4M iterations of `$p.bump` on a `has $.x is rw` class,
`taskset -c 2`, min of 11: **15.15s with the gate against 15.49s with it
skipped**, and 16.88s against 17.14s on a second interleaved round taken under
heavier machine load. The gate is *faster* in both rounds, which is the honest
reading of "below the noise floor": the sign is meaningless, and the
between-round drift (15.2s -> 16.9s on the same binary at the same switch
position) is an order of magnitude larger than the difference being measured.
Compare slice 3a, whose un-filtered gate showed a clean, repeatable **+13%** on
the same kind of A/B — a real cost is visible in this harness when there is one.

**What still refuses or is still wrong, all measured, all deliberately out of
scope.**

- **I3/K3 — a *subscript* receiver (`@a[0].mut`, `%h<a>.mut`) is unchanged.**
  This is the row the ADR named as 3b acceptance, and it is not reachable from
  this mechanism: `--dump-bytecode` shows `@a[0].mut` compiling to
  `GetArrayVar; LoadConst; Index; CallMethod` — a plain `CallMethod`, which
  carries **no** `target_name_idx`, and whose receiver arrives as a value the
  `Index` op has already read out of the array. There is no name to box and no
  location on the stack. What it needs is a *producer* — the subscript handing
  over the element's own cell, which `array_slot_ref` can already mint — and
  emitting that is an **unconditional compile-side change** to a very common
  shape (every `<subscript>.method(...)` in every program), so it must be
  paired with a decontainerize-at-the-chokepoint guard in `CallMethod` and
  re-measured, exactly as the E6 producer must. That is its own slice, recorded
  as `todo/tickets/subscript-receiver-raw-invocant-producer.md`. These rows are
  *unchanged*, not newly wrong: mutsu silently dropped the write before this
  slice and still does.
- **N1 — an attribute-accessor receiver (`$d.v.mut`) is unchanged**, for the
  same reason and by the same missing producer as 3a's E6 row
  (`MarkAccessorRefContext`). It belongs to that slice, not this one.
- **O1 — an aggregate receiver (`@a.mut` with `S = [7,8]`) is unchanged.**
  `capture_lvalue_invocant_cell` declines every route for an `@`/`%` name by
  design (route 4's scalar restriction), because a cell over an aggregate would
  disagree with its identity-shared storage. raku answers `[7 8]`; mutsu
  silently answers `[1 2]`, as before.
- **L4/L5/J5 — a non-raw invocant assigned to inside the body.** raku dies;
  mutsu silently drops the write, unchanged. This is a readonly-parameter
  enforcement gap, not an arrival gap: the gate correctly declines these, and
  the *observable* half of the contract (the caller's variable is not modified)
  already matches, which is what the pin asserts.
- **M1/M2 — an rvalue invocant (`42.mut`, `($a + 1).mut`).** raku dies with
  `Cannot modify an immutable Int`; mutsu silently succeeds doing nothing.
  Unchanged: there is no location, so the gate declines and nothing is boxed.

**Pinned by** `t/raw-invocant-arrives-as-container.t` (26 tests, byte-identical
output under `mutsu` and `raku`): the three raw-invocant spellings over both
binders, the routine-rw-capability independence (Correction A's B1/B2 rows),
`augment class Int`/`Str` receivers with a repeated mutation that proves the
container survives, each frame shape a receiver name can have (a `<->` and an
`is rw` loop parameter, a captured-outer scalar written from a closure, an
`is rw` sub parameter, a `:=`-bound alias), `multi` candidate selection, role
composition, the runtime method-name spelling, a body that reads its invocant
before replacing it, and five regression rows — the two non-raw invocant
controls, an ordinary method whose value semantics must survive the
program-wide pre-filter being on, and a read-only raw-invocant method that
must stay a plain rvalue call.

#### The E6 producer — IMPLEMENTED 2026-09-06

The slice 3a paragraph above split this row off with a prescription: emit
`MarkAccessorRefContext` before an lvalue invocant, pair it with the
decontainerize chokepoint, re-measure every `$obj.acc.m = v` shape. Every row of
the Correction-3 and slice-3a tables plus a fresh 29-row `$obj.acc.m = v` survey
was re-measured against raku v2026.07 and a debug `mutsu` built from `main` at
`30d6754f5` before any code was written. The prescription held; two things
around it did not.

**What shipped.**

- **The emission** (`Compiler::lvalue_invocant_wants_accessor_ref`,
  `src/compiler/expr_call.rs`), in the generic `CallFunc` argument loop: when a
  `__mutsu_assign_method_lvalue` call's argument 0 is an argument-less,
  unmodified, unquoted `Expr::MethodCall`, insert the marker before that call's
  trailing `CallMethod`/`CallMethodMut`. The insertion helper is slice 4's
  existing `mark_trailing_method_call_as_accessor_ref`, refactored to take the
  marker so both spellings share one site.
- **The op is `MarkLvalueInvocantRefContext`, not `MarkAccessorRefContext`** —
  a *runtime-gated* twin. See the cost note below for why. It sets the same
  `accessor_ref_pending` flag, deliberately: the consumer
  (`try_fast_accessor_read`'s `want_ref` branch) must stay one code path, or the
  container a `:=` bind gets and the container an lvalue invocant gets could
  drift apart.
- **No new consumer.** Slice 3a's `try_raw_invocant_container_lvalue` already
  writes through a `ContainerRef` invocant, and its gate
  (`box_raw_lvalue_invocant`) already returns early when `args[0]` is one, so
  the container is neither double-boxed nor re-derived. This is part 4 of the
  ADR working exactly as claimed: three producers now, one consumer.

**The narrowness is in the consumer, not the emission — which is what makes an
unconditional compile-side change safe.** Rawness is not statically known, so
the marker is emitted for every `$obj.acc.m = v`. But
`try_fast_accessor_read` hands back a container only for a zero-argument read of
a **public `is rw` scalar** attribute accessor (`rw_accessor_type_constraint`
is `Some` only for those, and `Array`/`Hash`/`Mixin` values are excluded), and
ignores the flag entirely otherwise. Verified with `--dump-bytecode`:
`benchmarks/method-call.raku`, `benchmarks/bench-class.raku` and a
`$p.x = $i` loop compile **byte-identically** with and without the change.

**Slice 3a's "single chokepoint" claim was not actually true, and E6 is what
exposed it.** 3a records that the target is decontainerized "at a single
chokepoint right after the branch declines", which "keeps the boxing invisible
to the other ~40 branches". One `Instance`-matching branch sat *above* it: the
IO::Path `.SPEC`/`.CWD` read-only guard. With a container invocant
(`class C { has IO::Path $.p is rw }; $c.p.SPEC = 5`) it stopped matching and the
diagnostic degraded from `Cannot modify an immutable IO::Spec::Unix
((IO::Spec::Unix))` to a bare `No matching candidates for method: SPEC`. The
guard now sits **below** the chokepoint, where the ADR's own rule always said it
belonged; neither branch above it cares (the type-object half requires a
`Package`, the raw-invocant half requires a raw-invocant declaration, and
`.SPEC` is neither).

**A cost that was measured, and designed out rather than paid — in three
steps, each of which the previous step's measurement forced.** All numbers are
same-binary env-switch A/B on a release build (comparing two binaries is not
reliable), min-of-N with the two arms **interleaved** so load drift hits both.

1. **The plain `MarkAccessorRefContext` cost ~14%** on a tight `$o.i.w = $n`
   loop (`class I { has $.w is rw }; class O { has $.i is rw }`). The extra
   opcode is not the cost: the `want_ref` branch runs
   `rw_accessor_type_constraint` (a `collect_class_attributes` plus an MRO walk)
   and `promote_attr_to_container` on every iteration, to mint a container the
   chokepoint then throws away because `.w` is not raw.
2. **A name-blind gate was not enough, and cost a heap allocation.** Gating on
   "could *any* callee be raw" (`Registry::any_raw_invocant_method` plus the
   native table) fixed the 6.d case but left 6.e paying the full price, because
   the native `snitch` row exists there for every method name. Worse, asking the
   native table called `current_language_version()`, which **clones a `String`**
   — a heap allocation per iteration, measured at ~29%. That is now
   `current_language_version_starts_with`, a non-allocating prefix check on the
   same thread-local.
3. **The gate carries the outer method's name**, which the compiler does know in
   every spelling but the dynamic one. The runtime test is then
   character-for-character slice 3a's own filter
   (`native_method_returns_raw_invocant(name) || any_raw_invocant_method`),
   evaluated one op earlier, and `$o.i.w = $n` declines even under 6.e —
   confirmed under `rust-gdb` by breaking on the flag-setting line and observing
   it never fire.

**Final numbers** (min of 15 interleaved pairs, on a box under sibling load):
`$o.i.w = $n` under 6.e **+1.9%**, the same loop under 6.d **-4.4%**. The noise
floor is calibrated by the control: `$p.x = $i` compiles **byte-identically** in
both arms (`--dump-bytecode` diff, as do `benchmarks/method-call.raku` and
`benchmarks/bench-class.raku`) and still measured **+6.3%**, so both figures are
inside the noise. `method-call.raku` and `bench-class.raku` measured 0.0%.

**What went green.** E6 itself (`$c.v.snitch = 9` -> `42` then `9`), its typed,
`Str`-valued, unset, inherited, role-composed and `self`-rooted twins, the
depth-2 accessor chain (`$o.i.w.snitch = 9`), and the *user*-declared
raw-invocant callees over the same producer — all three rw-capable spellings
(`is raw`, `is rw`, `return-rw`) reached through `augment class Any`.

**Still refusing after the E6 producer, all loudly, all measured:** the three
contract controls (a non-rw attribute accessor, a raw-invocant routine that is
not rw-capable, and an rw-capable routine whose invocant is not raw) plus a
raw-invocant body that returns a value rather than a location; `.self` over a
container invocant (an ADR non-goal); a computed invocant
(`method thing { 42 }`); `$c.a[0].snitch = 9` (an lvalue invocant that is an
*element* of an attribute-held array, whose compiled tail is `Index`, not a
method call, so no marker is inserted); and everything slice 3a already listed.

**Two rows the ADR's tables did not contain, both recorded rather than fixed:**

- An `is rw` **method** (not an attribute accessor) as the lvalue invocant —
  `class C { has $.v is rw; method acc is rw { $!v } }; $c.acc.snitch = 9` — is
  out of reach, because `try_fast_accessor_read` bails as soon as the name
  resolves to a `UserMethod` rather than an `Accessor`. The `:=` spelling of the
  same read is broken too (`my $x := $c.acc` dies), so this is a missing
  producer, not a missing consumer:
  `todo/tickets/rw-method-result-is-not-a-container-for-bind-or-invocant.md`.
- Argument position is a third consumer that still loses the container, and its
  `is raw` twin is **silently wrong**:
  `todo/tickets/attribute-accessor-container-lost-in-argument-position.md`. This
  is also where slice 3a's discriminator row was misattributed — see the
  correction note above.

**Pinned by** `t/lvalue-invocant-attribute-accessor-container.t` (25 tests) and
`t/lvalue-invocant-user-raw-method.t` (8 tests), both byte-identical under
`mutsu` and `raku`. `.snitch` is given an explicit snitcher throughout so the
observation lands in a variable rather than on stderr, which keeps the
comparison over stdout alone. Between them they cover every green row above, the
six controls, the runtime method-name spelling (the one shape whose marker
carries no name, so its gate must pass), and the shapes that had to stay
untouched: the plain `$c.v = 9`
store, the `:=` bind producer, an rvalue accessor read still copying, a nested
non-rw attribute store (and the object still rendering as itself, since the
producer promotes attribute slots to shared cells), array- and hash-valued
attribute element stores, and an argument-carrying rw accessor invocant.

#### The returned-container consumers — IMPLEMENTED 2026-09-06

The E6 producer closed with two rows recorded rather than fixed — an `is rw`
*method* (not an attribute accessor) as an lvalue invocant, and argument
position as a third consumer — and filed them as
`todo/tickets/rw-method-result-is-not-a-container-for-bind-or-invocant.md` and
`todo/tickets/attribute-accessor-container-lost-in-argument-position.md`. Every
row of both was re-measured against raku v2026.07 and a debug `mutsu` built from
`main` at `1e946f7c8` before any code was written, along with a survey of the
*other* rw-tail shapes that both tickets took for granted. All six broken rows
still held exactly as filed. **Both tickets' diagnoses of where the work was did not.**

| # | Program (`class C { has $.v is rw; method acc is rw { $!v } }`) | raku | mutsu (before) |
|---|---|---|---|
| T1 | `$c.acc = 9` | `9` | `9` |
| T2 | `my $x := $c.acc; $x = 9` | `9` | dies, `Cannot assign to an immutable value` |
| T3 | `sub g($y is rw) {...}; g($c.acc)` | `9` | dies, `expects a writable container` |
| T4 | `$c.acc.snitch = 9` | `42` / `9` | dies, `X::Assignment::RO: … on non-instance` |
| T5 | the same through `method !p is rw { $!v }` and `self!p` | `42` / `9` | the same refusal |
| U1 | `my $x := $c.v; $x = 9` (plain accessor) | `9` | `9` |
| U2 | `$c.v.snitch = 9` (the E6 row) | `42` / `9` | `42` / `9` |
| U3 | `sub g($y is rw) {...}; g($c.v)` | `9` | dies, `expects a writable container` |
| U4 | `sub f(\x) is raw { x }; f($c.v) = 9` | `9` | **`42` — silent, exit 0** |

**Correction 1 — the `is rw` method rows are not a producer question at all,
and the ticket's prescription would have built machinery that is not needed.**
The ticket proposed "a producer that *runs* the rw method for a plain read and
hands its container back when the read is in a container-wanting context",
gated on `method_is_rw_capable`, with the `:=` row as "the cheapest entry
point". Measuring the method's *other* tail shapes says the container-wanting
context has nothing to do with it:

| # | Program | raku | mutsu (before) |
|---|---|---|---|
| S3 | `class D { method m(\x) is rw { x } }; $d.m($a).VAR.^name` | `Scalar` | `Scalar` |
| S4 | `my $y := $d.m($a); $y = 7` | `7` | `7` |
| S5 | `sub g($p is rw) {...}; g($d.m($a))` | `8` | `8` |
| S7 | `class E { has @.l is rw; method at($i) is rw { @!l[$i] } }; my $z := $e.at(1); $z = 5` | `[1 5]` | `[1 5]` |
| S2 | `$c.acc.VAR.^name` for the `{ $!v }` tail | `Scalar` | **`Int`** |

An `is rw` method already hands back a location for a sigil-less `\x` tail
(slice 1's `CaptureVarCell`) and for an `@!l[$i]` tail (the subscript's own
container-mode compile), and every consumer — `:=`, an `is rw` argument, an
lvalue invocant — already accepts it. **Exactly one tail shape was left out:
the bare `$!v`.** Not a call-site producer, a hole in the *callee's* compile.

The reason it was left out is structural rather than accidental. A method frame
does not read the attribute out of the instance; dispatch **seeds a local slot**
named `!v` with a copy, and the tail compiles to `GetLocal(<that slot>)`. So
`CaptureVarCell` — which boxes a frame slot — would have minted a cell
disconnected from the instance, and writes through it would have evaporated
silently. That is why the shape needs an op of its own rather than the existing
capture.

**Correction 2 — argument position needed no new absorbing chokepoint, and the
one place that did not absorb a container was a pre-existing hole reachable
without this change.** The ticket's stated blocker was that "unlike the invocant
case there is no decontainerize chokepoint downstream to absorb a `ContainerRef`
that nobody consumes — so this needs its own measurement of where such a
container would flow before any code is written". Measured, the binder already
absorbs it: `binding_signature.rs` has an explicit arm saying "a bare
`ContainerRef` cell … IS a writable lvalue even without a source variable name",
and a read-only / `is copy` parameter decontainerizes on its way in. The single
consumer that did **not** was multi-*dispatch* candidate matching — and it was
already broken for every other container producer:

| # | Program | raku | mutsu (before) |
|---|---|---|---|
| X1 | `multi mm(Int $y is rw) {…}; multi mm(Str $y) {…}; mm($d.acc($a))` | `5` | dies, `Cannot resolve caller mm(Int:D)` |
| X2 | the same over `mm(relay($b))` for `sub relay(\x) is raw { x }` | `5` | the same |

X1/X2 need no accessor and no part of this slice to reproduce; they are the
argument twin of the "single chokepoint that is not single" lesson E6 recorded.
Fixing them is what keeps this slice from trading a working `g($c.v)` for a
broken `mm($c.v)`.

**What shipped.**

- **`OpCode::AttrContainerRef(name_idx)`** and `src/vm/vm_rw_attr_container.rs`.
  Emitted by `compile_return_rw_arg` for an `Expr::Var("!attr")` rw tail, it
  reaches past the seeded slot to `self`'s own attribute cell and calls
  `promote_attr_to_container` — character-for-character the promotion
  `try_fast_accessor_read`'s `want_ref` branch makes for a public accessor.
  Sharing the promotion rather than minting a second cell is what makes
  `my $x := $c.v` and `my $x := $c.acc` name **one** container when `acc`
  exposes `v` (pinned: `$c.acc =:= $c.acc` is `True`). A private-only
  `has $!priv` is reached under its `priv!` storage key, and the declared type
  travels with the cell through a dedicated MRO walk rather than through
  `rw_accessor_type_constraint`, which requires a *public* `is rw` accessor and
  so would have dropped the constraint for exactly the private case an `is rw`
  method exists to expose.
- **`OpCode::MarkRwArgRefContext { callee_idx, positional }`** and
  `src/runtime/rw_arg_container.rs`. The E6 producer's marker, emitted before a
  positional *argument* that is an argument-less, unmodified, unquoted method
  call, and runtime-gated on whether any registered candidate of the named
  callee declares a container-binding parameter at that index
  (`ParamDef::binds_caller_container`, the *same* predicate the binder uses to
  decide whether to install the shared cell). It sets the same
  `accessor_ref_pending` flag as the other two producers, deliberately: the
  consumer must stay one code path.
- **The parser-rewritten lvalue spellings are relayed, not special-cased.**
  `f($c.v) = 9` and `++f($c.v)` are not compiled as calls to `f` at all — the
  parser rewrites them to `__mutsu_assign_named_sub_lvalue("f", [ARGS], value)`
  / `__mutsu_incdec_named_sub_lvalue(…)`, whose real callee is a *string
  argument* and whose real arguments sit inside a list literal. One compiler
  field (`pending_rw_arg_list_callee`) carries the real callee's name down to
  the list-literal element loop, which marks each element with that callee and
  its own positional index. Both helpers resolve their routine at run time for
  the same reason this gate does — a routine may be declared after its use site.
- **Multi dispatch type-checks a container by its contents.**
  `args_match_param_types` now derefs a `ContainerRef` argument before the type
  constraint runs, and its `is rw` dispatch gate accepts a bare `ContainerRef`
  as the writable lvalue the binder already says it is. Before this the matcher
  rejected every typed signature and reported `Cannot resolve caller mm(Int:D)`
  — naming the very type it had just refused to match, because the *message*
  deref'd and the matcher did not.
- **No new consumer, again.** `assign_lvalue_container` (ADR-0059), the binder's
  bare-`ContainerRef` arm, and slice 3a's `try_raw_invocant_container_lvalue`
  all consume these containers unchanged. Part 4 of this ADR now stands at five
  producers and the same consumers.

**The battery gate found what `make test` and roast both missed, twice — and
both leaks were pre-existing holes this slice merely made reachable.** The
`is rw` method producer makes an `$!attr`-tailed method genuinely return a
container, which is what raku does (`$c.acc.VAR.^name` is `Scalar`). Four
whitelisted `URI` files then regressed, and reducing them exposed two distinct
places where a `ContainerRef` was NOT transparent. Both reproduce on `main`
through the *existing* producers, with no part of this slice involved:

| # | Program | raku | mutsu (before) |
|---|---|---|---|
| G1 | `class U { has A $.a is rw; method m { with $!a {...} } }` after the slot is promoted | takes the `else` branch | entered `with` on the *cell*, then died assigning through the topic |
| G2 | `class T { method Str {'s'} }; sub f(\x) is raw { x }; ~f($t)` | `s` | `T()` |
| G3 | the same under `say f($t)`, `"{ f($t) }"`, and `is f($t), 's'` | `s` | `T()` |

**G1 — an attribute slot promoted to a cell must still read and write as an
attribute.** `promote_attr_to_container` replaces the slot with a
`ContainerRef`, and the method body's cell-direct `$!x` read
(`read_attr_cell_by_key`, `vm_var_assign_computed_attr.rs`) handed that cell
back undereferenced — indistinguishable from a defined value, so
`with $!authority { ... }` entered on an attribute holding a type object and
made the topic the cell rather than the object. The write side had the mirror
bug: `write_attr_cell_by_key` *replaced* the slot, which would disconnect every
alias already handed out of it at the first internal `$!x = v`. Both are fixed
at the primitive: the read derefs, and the write goes through
`InstanceAttrs::store_through_container`, which stores into the cell when the
slot holds one. This is the same rule ADR-0013 states for a container generally
— the cell IS the attribute's Scalar — and it was simply not applied to the
cell-direct path, because before this slice a promoted attribute was rare
enough (only a `:=` bind to an accessor) that no bundled library hit it.

**G2/G3 — a `ContainerRef` was transparent to `Value::to_string_value` but not
to the four *user-method-aware* renderers.** `~`, `say`/`note`,
string interpolation and the `Test` assertions each decide between a pure
stringifier and a `.Str`/`.gist` dispatch by matching the value's shape, and
none of them looked through a container — so an `Instance` inside one rendered
as the pure `TypeName()` placeholder and the user's `method Str` never ran.
Fixed at each of the four (`exec_str_coerce_op`, `needs_method_dispatch` +
`render_gist_value` / `render_str_value`, `exec_string_concat_op`, and
`unwrap_test_arg_value`, which already unwrapped the sibling `VarRef` wrapper
for exactly this reason). These are four named chokepoints, not a campaign:
each is the single place its spelling decides how to render.

**The lesson E6 recorded holds again, in a wider form.** E6 found one
`Instance`-matching branch sitting above slice 3a's decontainerize chokepoint.
Here the same shape appears five more times, in code that has nothing to do with
lvalues — every site that *dispatches on a value's shape* is a place a container
can be mistaken for the thing it holds. The gate is what surfaced them: `make
test` and the 326-file targeted roast sweep were both green while all five were
broken.

**Cost: the gate is compiled out of every program that does not use the shape.**
`AttrContainerRef` exists only inside an `is rw`/`is raw` method body whose tail
is a bare `$!attr`, so it cannot appear on a path that did not already declare
one. `MarkRwArgRefContext` is emitted only for an argument-less method-call
argument, which is why **every file in `benchmarks/` compiles to bytecode
containing neither op** (checked with `--dump-bytecode` across all 23). There is
therefore no A/B to report and none is claimed: on this box a byte-identical
control drifted +6.3% during the E6 measurement, so anything under ~7% would be
unreadable anyway — the honest statement is that the benchmark programs are
byte-identical, not that a difference was measured and found small.

**Two argument spellings are still not covered, both refusing loudly, both
measured and deliberately out of scope.** A *method*-call argument
(`$s.take($c.v)` for `method take($y is rw)`) and a call through a code
variable (`my $r = &g; $r($c.v)`) both die with "expects a writable container"
where raku answers `9`. Neither has a callee name the gate can key on: the
method case would need a **name-only** index over the registry's
`(owner, name)`-keyed method table, because the invocant's class is not knowable
at compile time, and the code-variable case has no name at all. The available
cheap over-approximation for the method half (a program-wide set-only "any user
method declares a container-binding parameter" flag, slice 3a's
`any_raw_invocant_method` pattern) would produce a container for *every*
accessor-shaped method argument in any program that declares one such method
anywhere — a far wider behaviour change than this slice's callee-keyed gate, and
one that wants its own measurement. Note the direction: both keep refusing, so
neither is a silent wrong answer. Recorded as
`todo/tickets/rw-argument-producer-needs-a-nameless-callee-gate.md`.

**Still refusing, all loudly, all measured:** a non-rw attribute accessor as an
`is rw` argument (`g($p.v)` for `has $.v`), a non-rw-capable method result
(`g($c.plain)`), an rw-capable method that returns a value rather than a
location (`g($c.value)`), a literal (`g(42)`), and assigning to a non-rw method
result (`$c.plain = 1`) — the five controls the pin asserts, with unchanged
messages. An `@`/`%`-sigiled rw tail (`method aggregate is rw { @!l }`) is
deliberately untouched: an aggregate attribute value is already a shared
container reached by its own accessor path, and wrapping it in a scalar cell
would disagree with that storage — the same restriction slice 3a's route 4 and
`try_fast_accessor_read` both apply.

**Two residuals, measured, and NOT caused by this slice.** Assigning `Nil`
through an `is rw` method (`$u.auth = Nil` for `method auth is rw { $!a }` on a
typed `has A $.a`) leaves the attribute holding `Nil` where raku restores the
declared type object — the accessor store does it right, the rw-method store
writes the value straight into the attribute map and skips the reset. And
`sub f(\x) is raw { x }; f(<non-location>) = 9` reports success and drops the
write where raku dies — `f(42) = 9` reproduces it with no accessor anywhere, so
it is the assignment path failing to refuse a routine that handed back a value,
not an argument-producer gap. Unchanged by this slice.

Both were fixed on 2026-09-06 (together with a third, unrelated `@`-attribute
list-assignment bug found in the same neighbourhood) — see
`news/2026-09/attribute-and-raw-lvalue-stores-share-one-rule.md`. The rw-return
container capture (`OpCode::CaptureVarCell`) now asks the readonly registry
before minting a cell, so a routine that handed back a value is refused instead
of writing into a container nobody shares; and the rw-method attribute store
shares the accessor's `Nil`-reset and type-check rules
(`attr_store_nil_default` / `check_attr_store_type`) rather than carrying its
own partial copies.

**Pinned by** `t/rw-result-container-consumers.t` (36 tests, byte-identical
output under `mutsu` and `raku`): every row of both tickets, the four `is rw`
method consumers over scalar / typed / private-only / defaulting-body
attributes, the container identity (`=:=`) that proves the accessor and the
method name one cell, the aggregate tail, both multi-dispatch rows, the
`is copy` / read-only argument shapes that must keep copying, repeated argument
binding through the same accessor, and the five refusal controls.

### Slice 4 — the chain walk steps through an object

Part 5, variable-rooted half: replace
`vm_var_assign_index_named.rs:2985`'s rvalue `AT-KEY`/`AT-POS` call with the
lvalue-mode call, and descend into the returned container.

**Acceptance:** C3/H1, C4 (must stay correct), H2, H5 (depth 3 — must stop
replacing the instance with a Hash), and C2 (the `AT-POS` twin). H3/H4 are
regression rows.

#### Slice 4 — IMPLEMENTED 2026-09-05

Every row in the Correction-3 table was re-measured against raku v2026.07 and a
debug `mutsu` built from `main` at `895d8abc3` before any code was written. All
of them still held exactly as written, loud refusal and silent drops alike.

**What shipped**, all in the new `src/vm/vm_lvalue_object_subscript.rs`
plus three call sites in `vm_var_assign_index_named.rs`:

- **`object_subscript_accessor`** — the accessor an object serves a step with,
  extracted verbatim from the two-level walker's own `AT-POS`/`AT-KEY`
  primary/secondary probe so both walkers ask the same question.
- **`lvalue_object_step_container`** — the container a deeper subscript must
  walk, given whatever the accessor returned. A `ContainerRef` cell or a
  `HashEntryRef` token holding a container hands that container back (it shares
  its `Gc` node with the object's own storage, so a write through it reaches the
  object with no write-back); an *empty* location autovivifies a container of
  the kind the **next** step addresses and installs it there, which is what
  makes `$q<new>[0] = 9` grow `{new => [9]}` and `$p[2][0] = 9` grow the array.
- **The two-level op** now calls the accessor **once** and keeps both its
  container and its value. The `ASSIGN-POS`/`ASSIGN-KEY` and `Proxy`-element
  branches are unchanged and still run first; only when both decline does the
  walk store through the returned location instead of falling out to the generic
  Hash/Array walk against a root that is not a container.
- **The deep (3+ level) op** takes the same step at every intermediate level,
  keeping each produced container in a `Vec<Box<Value>>` so the raw-pointer walk
  has a stable, kept-alive address to descend into. This is what stops H5
  replacing the object with a fresh Hash.
- **The generic (stack-computed target) op** gained a `ContainerRef` arm that
  resolves the cell exactly as its existing `HashEntryRef` arm resolves a
  deferred entry. That is H2, `$q.AT-KEY("foo")[0] = 99`: an explicit accessor
  call is not rewritten into a chain-root temp, so its container arrived here and
  was dropped by the catch-all arm.

**The discriminator is the shape of what the accessor returned, not a
declaration probe.** No `routine_is_rw_capable` call was needed: a rw-capable
`AT-KEY` body is already compiled with an rw tail (slice 2 widened that to
`is_rw || is_raw`, slice 1 made a sigil-less tail denote its container), so the
call already hands back a location. That is precisely why the `:=`-bound
spelling H3 has always worked — the producer existed and simply was not
consulted. An accessor that is *not* rw-capable returns a plain value and every
caller keeps its previous behaviour.

**One row was measured that the ADR's table did not contain, and it is fixed
too.** `class R { has %.d; method AT-KEY($k) { %!d{$k} } }; $r<foo>[0] = 9` is
`{foo => [9 2]}` in raku even though the accessor is **not** rw: raku mutates
the returned `Array` *object* in place, and mutsu's method return shares its
`Gc` node, so the same is true here. `lvalue_object_step_container` therefore
also accepts a bare `Array`/`Hash` return. Without that row the fix would have
read as "rw accessors only", which is not what raku does.

**Slice 5 shrank as a result, measured.** B1 (the ticket's headline) and B6 turn
out to be *variable*-rooted once the compiler has run: `--dump-bytecode` shows
`$u.query<foo>[0] = 99` compiling to `SetGlobal(__mutsu_lvroot_%query#4)`
followed by `IndexAssignExprNested`, i.e. the two-level walker with the object
sitting in a chain-root temp — and the walker's new branch returns before
`lvalue_root_temp_not_a_container`'s refusal is ever reached. Both are green
after slice 4 and are pinned here. What is left for slice 5 is B4
(`$u.query<foo> = 99`, depth 1), which is a different function entirely
(`__mutsu_index_assign_method_lvalue` in `builtins_multidim_assign.rs`, arity 5),
and the deep op's own root-temp refusal for a depth-3 method-rooted chain.

**Pinned by** `t/lvalue-subscript-chain-through-object.t` (16 tests,
byte-identical output under `mutsu` and `raku`): the five acceptance rows, the
two `:=`-rooted spellings, the three autovivification shapes (missing hash key,
out-of-range `AT-POS`, hash-valued element), the non-rw accessor, and five
regression rows — H3, H4, an inner `ASSIGN-KEY` object still winning the
outermost write, a plain `Hash` root, and plain deep autovivification.

**Two residual divergences, both measured, both left alone deliberately:**

- `my $q = Q.new(d => {foo => 1}); $q<foo>[0] = 9` — raku dies with "Cannot
  modify an immutable Int (1)"; mutsu silently does nothing, exactly as before.
  `lvalue_object_step_container` answers `None` for a location holding a defined
  non-container rather than vivifying over real data, so this row is unchanged
  rather than newly wrong.
- `$a<zz> = 5` on a class supplying `ASSIGN-KEY` — raku calls `ASSIGN-KEY`
  (`zz => A:5`), mutsu stores `5` directly. That is the **single-level** named
  store, not a chain, so it is a different site from anything this slice
  touches.

### Slice 5 — the method-rooted chain root

Part 5, method-rooted half: compile
`bind_method_rooted_chain_root`'s root expression in container mode.

**Acceptance:** B1 (the ticket's headline), B4 (depth 1), B6 (the `:=`-bound
alias spelling), with
`t/method-rooted-lvalue-subscript-chain.t` as the regression gate.

#### Slice 5 — IMPLEMENTED 2026-09-05

**The prescription above is wrong, and measurement is what showed it.** Nothing
had to be compiled in container mode. `--dump-bytecode` on B1 shows the chain
root already reaching the walker as a *value* the walker can step through:

```
GetLocal(0); CallMethodMut{name_idx:3 "query"}; SetGlobal(4 "__mutsu_lvroot_%query#4")
LoadConst 99; LoadConst 0; LoadConst "foo"; IndexAssignExprNested{name_idx:4}
```

The temp holds the `Q` **object**, and slice 4 taught the walkers to step
through an object. So B1 and B6 needed no slice-5 code at all — they went green
with slice 4 and are pinned there. What was actually left was two sites that
refuse or ignore an object root:

- **`lvalue_root_temp_not_a_container`** (the loud
  "it returned Q, not an Array or Hash container") accepted only `Array`/`Hash`.
  It now also accepts a root that supplies `AT-KEY`/`AT-POS`, since that *is* a
  location the walk can step through. This is what B8 —
  `$u.query<foo><bar>[0] = 99`, the depth-3 method-rooted chain, which the deep
  op refuses before walking — needed.
- **`builtin_index_assign_method_lvalue`** (`__mutsu_index_assign_method_lvalue`,
  the depth-1 method-rooted store — a *different function* from either walker)
  dispatched `ASSIGN-KEY`/`ASSIGN-POS` on an object accessor result and, when
  the class had neither, fell into plain-container handling and dropped the
  write. It now calls the object's own `AT-KEY`/`AT-POS` and hands the returned
  location to **`assign_lvalue_container`** — ADR-0059's consumer, unchanged, as
  part 4 of this ADR promised. B4, B9 (autovivifying a key the object does not
  have) and B10 (the `AT-POS` twin).

That last site is gated on **rw-capability** (`method_is_rw_capable`, slice 2's
oracle) rather than on the shape of what came back, because reaching the shape
means *calling* the accessor: a non-rw accessor is one raku refuses to assign
through anyway, so calling it would add a side effect for nothing. The walkers
can afford the opposite policy (shape, not declaration) because they were
already calling the accessor.

**Also shipped, from re-reading slice 4's own diff rather than from a repro.**
The deep op's object step calls user code while `current` is a raw pointer into
`self.env`'s value slot; an `env` insert from inside the accessor can rehash the
map and dangle it. The success path already continued from an owned
`Box<Value>`, but the "produced no location" path fell through to the generic
`&mut *current` arms with the pre-call pointer. Both paths now continue from an
owned value — and for the failing one that is also the better answer, since the
generic arms then overwrite a local binding instead of the real element.

**The whole repro set of the originating ticket now matches raku**, so
`todo/tickets/lvalue-chain-through-at-key-at-pos-object-root.md` is closed to
`news/2026-09/`.

**Pinned by** `t/method-rooted-lvalue-subscript-through-object.t` (12 tests,
byte-identical under `mutsu` and `raku`): B4/B9/B10 at depth 1, B1/B8/B6 deeper,
and six regression rows — an inner `ASSIGN-KEY` object still winning, plain
hash/array attributes at depth 1 and 2, and a genuinely non-location root
(`method thing { 42 }`) still refusing loudly.
`t/method-rooted-lvalue-subscript-chain.t` is unchanged and still green.

**One residual, measured:** `$w.p<a> = 1` where `Plain` supplies **no**
subscript accessor at all reports success and drops the write (raku: "Type Plain
does not support associative indexing"). The depth->=2 spelling of the same
shape already refuses loudly; only the depth-1 store is silent, and gating the
new branch on having an accessor leaves it exactly as it was rather than making
it newly wrong.

## Alternatives considered

- **Add `snitch` to `.item`'s compile-time erasure.** Rejected on correctness,
  not taste: `.item` is erased, and `.snitch` has a side effect that erasure
  would drop (raku prints the invocant before the assignment lands). Measured
  above.
- **Special-case `.VAR`.** Not applicable — `.VAR` is not in this family at all;
  raku refuses `$a.VAR = 5`. Correction 1.
- **Fix (b) with an accessor-keyed slow path.** Rejected — that is the deleted
  `__mutsu_index_assign_method_lvalue_nested`, and its copy-on-write rebuild is
  precisely what dropped the writes. Explicitly forbidden by the ticket.
- **Make the *whole* invocant path container-carrying, unconditionally.**
  Rejected: it would leak a `ContainerRef` into every method body and past every
  consumer that only decontainerizes at the scalar chokepoints — the same
  failure mode `return_rw_container_name`'s narrowness exists to avoid. Rawness
  is a declared property; only declared-raw invocants get the container.
- **Wait for a "universal container-reference propagation" campaign** (the deep
  file's 2026-08-31 triage). Rejected as over-scoped: the measurements above
  show mutsu already propagates containers through positional raw parameters
  (F3/G3), through `:=`-bound subscripts (H3), and out of `return-rw` tails.
  Three named gaps remain, each with its own repro. There is no universal
  campaign left to run — there are four slices.

## Non-goals

- `$a.VAR = 5` stays a refusal (it is one in raku).
- Hyper method calls (`>>.`) keep decontainerizing.
- `.self` is *not* in the raw-invocant family in raku (`$a.self =:= $a` is
  `False`, and `$a.self = 5` is refused). It is deliberately outside this ADR's
  table; the divergence it once carried was fixed separately, see
  `news/2026-09/self-method-decontainerizes.md`.
- ~~`sub f(\x) is raw { x }; my $c = C.new(v=>1); f($c.v) = 9` (a raw *argument*
  over an attribute accessor) still copies. It is the argument twin of Slice 3
  and is expected to fall out of it; if it does not, it earns its own ticket.~~
  **Resolved 2026-09-06** by the returned-container-consumers slice above. Two
  things this bullet said were wrong, both measured: it did not "copy", it
  reported success and dropped the write; and it did *not* fall out of slice 3,
  it earned its ticket and its own argument-position producer.
