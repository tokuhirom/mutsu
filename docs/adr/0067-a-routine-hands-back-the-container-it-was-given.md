# ADR-0067: A routine hands back the container it was *given* — raw arguments, raw invocants, and the subscript step through an object

- Status: Proposed (Slices 1, 2, 3a and 4 implemented 2026-09-05; Slice 3 was
  re-scoped into 3a/3b on the same day after measurement, and 3a's E6 row split
  off again into the rw-attribute-accessor producer; Slice 4 absorbed two of
  Slice 5's three acceptance rows, again after measurement; Slices 3b, 5 and the
  E6 producer open)
- Date: 2026-09-05
- Related: [ADR-0059](0059-is-rw-routines-return-a-container.md) (an `is rw`
  routine returns a container), [ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md)
  (element-container Pairs), [ADR-0013](0013-container-interior-mutability-cellvalue.md)
  §7 (interior-mutability prerequisite — solved),
  [ADR-0001](0001-gc-strategy-and-phasing.md) §7 (Track B is no longer
  GC-coupled), [ADR-0064](0064-var-descriptor-carries-the-contained-value.md)
  (`.VAR` descriptors)
- Addresses: `todo/deep/native-method-cannot-return-an-lvalue-container.md`,
  `todo/tickets/lvalue-chain-through-at-key-at-pos-object-root.md`

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
produces a container for a `:=` bind but not in argument position
(`sub g($y is rw) {...}; g($c.v)` dies with "expects a writable container").

**Also still refusing after 3a, all loudly (not silently wrong), all out of
scope:** `@a.snitch = (7,8)` and `%h.snitch = (b=>2)` (aggregate invocants —
route 4's scalar restriction declines them), `$a.list = 5` (list assignment, see
above), `$a.snitch.snitch = 5` (a chained lvalue invocant),
`@n[0][1].mutsuRawInv = 8` (a depth-2 subscript invocant, which is slice 4's
walker), and `42.snitch = 5` (raku also dies, with a different message).

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
  `False`, and `$a.self = 5` is refused) while mutsu answers `True`. That is a
  separate divergence, recorded here so it is not swept into the table by
  mistake.
- `sub f(\x) is raw { x }; my $c = C.new(v=>1); f($c.v) = 9` (a raw *argument*
  over an attribute accessor) still copies. It is the argument twin of Slice 3
  and is expected to fall out of it; if it does not, it earns its own ticket.
