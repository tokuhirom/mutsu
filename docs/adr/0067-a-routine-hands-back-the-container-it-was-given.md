# ADR-0067: A routine hands back the container it was *given* — raw arguments, raw invocants, and the subscript step through an object

- Status: Proposed (Slice 1 implemented 2026-09-05; Slices 2-5 open)
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

### Slice 2 — one rw-capability oracle for methods

Part 1's user half: add `is_raw` to `Stmt::MethodDecl` and `MethodDef` (~18
construction sites), parse the trait, and replace
`methods_mut_method_lvalue.rs:1363`'s `!method_def.is_rw` with
`routine_is_rw_capable`. Mechanical and wide; no design left in it.

**Acceptance:** K2 and K4 above.

### Slice 3 — the invocant arrives as a container

Parts 1 (native table) and 2. Emit `CaptureVarCell` for the invocant of a call
to a raw-invocant routine at the lvalue call site, teach the runtime dispatch
not to deref it, and have the raw-invocant natives return it unchanged.

**Acceptance:** A3/A6 (`$a.snitch = 5` -> `42` then `5`; the `augment` twin),
I1/I3 (mutation *through* a raw invocant reaches the caller), and E4/E5/E6 (the
element and attribute invocant spellings). E1/E2 must keep refusing.

### Slice 4 — the chain walk steps through an object

Part 5, variable-rooted half: replace
`vm_var_assign_index_named.rs:2985`'s rvalue `AT-KEY`/`AT-POS` call with the
lvalue-mode call, and descend into the returned container.

**Acceptance:** C3/H1, C4 (must stay correct), H2, H5 (depth 3 — must stop
replacing the instance with a Hash), and C2 (the `AT-POS` twin). H3/H4 are
regression rows.

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
