# ADR-0084: The per-frame `Env` is not the program's symbol table

- Status: Proposed
- Date: 2026-09-09
- Related: [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md)
  (slot-addressed lexical capture and env synchronization),
  [ADR-0024](0024-mainline-lexicals-for-named-subs.md) (a mainline routine
  carries the lexical store of its compilation unit),
  [ADR-0039](0039-container-lexicals-resolve-lexically.md) (replace by-name
  lookup with an owning lexical scope), and
  [ADR-0081](0081-compunit-scoped-module-import-aliases.md) (a unit module's
  imported aliases are scoped to its compilation unit)
- Addresses: GitHub issues
  [#7667](https://github.com/tokuhirom/mutsu/issues/7667) (the measurements
  below), [#7787](https://github.com/tokuhirom/mutsu/issues/7787) (a module's
  `constant`s and enum values leak by this route), and the still-open
  divergence (b) of [#7555](https://github.com/tokuhirom/mutsu/issues/7555)
- Tracked by: [#7817](https://github.com/tokuhirom/mutsu/issues/7817)

## 1. Context

### 1.1 What is measured

`Env` is mutsu's per-frame lexical environment: an `Arc<FxHashMap<Symbol,
Value>>` that call frames clone, closures capture, and (until #7796) thread
clones deep-copied. It is copy-on-write, so a write to a *shared* env copies the
whole map.

Dumping the keys of one frame env at the moment it is deep-copied, in a program
that has done nothing but `use Cro::HTTP2::RequestParser` and build one
`Promise(supply { whenever … })` — **403 entries**:

| kind | count |
| --- | --- |
| `__mutsu_callable_id::Pkg::name` markers | **128** |
| qualified type/package names (`Cro::HTTP2::Frame`, `Cro::BodyParser`, …) | **159** |
| leading-capital type/enum names (`Any`, `PROTOCOL_ERROR`, `CANCEL`, …) | 51 |
| `__mutsu_constant_var::` markers | 27 |
| dynamics (`$*x`) | 20 |
| bare lowercase subs/terms | 8 |
| `__mutsu_type::` markers | 6 |
| **actual sigilled lexical variables** | **2** |

Two entries out of 403 are lexical variables. Deeper frames in the same program
reach ~790 entries with the same composition. The equivalent env in a program
with no `use` is 34 entries.

### 1.2 What it costs

The env is the structure everything copies, so its size is a multiplier on
several unrelated hot paths:

- **Frame setup.** `call_sub_value` builds a call frame's env with a
  `clone` → `insert` → `clone` → `insert` sequence; each insert taken while the
  map is shared copies all ~790 entries. Measured: 22 deep copies totalling
  17,453 entries per `Promise(supply { whenever … })`, ~1.95M instructions,
  roughly 0.5 ms.
- **Closure capture.** A closure's captured env is a flat snapshot of everything
  in scope, so creating one is O(program), not O(free variables) — even though
  the compiler already computes `free_var_syms`.
- **Thread spawn.** Fixed for the *program tables* by #7796, but the env itself
  is still copied per spawn lineage.
- **Identical code, different cost.** The same six lines of Raku cost 0.21 ms in
  a bare program and 1.17 ms after `use Cro::HTTP2::RequestParser`, with
  **identical opcode counts** — the whole difference is native-side copying that
  scales with how many names are in scope.

Per-site attribution for the 22 copies (entries copied per iteration):

```
  2384  resolution_call_sub.rs:784   __mutsu_callable_id insert after block_sub shares the env
  2383  resolution_call_sub.rs:769   &?BLOCK insert after block_arc shares the env
  2382  native_supply_methods.rs:284 sub_with_env_key
  2379  resolution_call_sub.rs:1014  persist_closure_env rebuild
  1584  types/binding_signature.rs:627
  1577  env.rs:1477
   796…788  methods_mut_substr_buf, methods_mut_dispatch, vm_var_assign_set_local,
            vm_register_ops, resolution_call_sub.rs:739 / :674
```

### 1.3 Why this is not a micro-optimization problem

The obvious local fix — reorder the `clone`/`insert` pairs so one deep copy
serves several inserts — was tried and **rejected on the spot**: the position of
the `__mutsu_callable_id` insert is load-bearing. `call_sub_value` decides
whether a non-local `return` propagates with

```rust
let has_target = e.return_target_callable_id().is_some()
    || data.env.contains_key("__mutsu_callable_id");
```

so moving that insert earlier puts the key into `block_sub`'s captured env, and
`block_sub` is reachable and callable as `&?BLOCK`. That changes `return`
semantics inside `&?BLOCK()` to buy ~0.09 ms. Under this repo's own definition
(CLAUDE.md, "What gain and risk actually mean") that is a risk, not a gain.

The remaining sites are the same shape: each is *individually* justified, and
each is expensive only because the map it copies is 400-790 entries of things
that are not lexical variables.

### 1.4 The correctness half

This is not only a performance question. The same storage choice is the root of
two open correctness divergences:

- #7555 divergence (b): `run_modules.rs`'s `new_types` → `package_type_aliases`
  write deliberately aliases every class a module load registered into the
  *importer's* package, to compensate for weaker lexical scoping. Rakudo does
  not make those names visible.
- #7787: a module's file-scope `constant`s and enum values reach the importer by
  the same route.

So the entries that dominate the copy cost are, in part, entries that should not
be visible at all.

## 2. Decision

**Proposed.** The per-frame `Env` holds *lexical variables and dynamics*. Type
and package names, a compunit's constants and enum values, and internal
per-callable bookkeeping move to package-/compunit-keyed side tables that
bareword and routine resolution consult directly, and that frames neither clone
nor capture.

mutsu already has the receiving structures — `package_type_aliases`,
`module_scope_lexicals`, `package_lexicals`, `unit_lexicals` — and, since #7796,
they are `Arc` copy-on-write, so moving entries into them makes those entries
free to carry across frames and spawns rather than merely cheaper.

Three separable groups, in increasing order of blast radius:

1. **`__mutsu_callable_id::Pkg::name` markers** (128 of 403). Internal
   registration markers, not user-visible names. They are deliberately *lexical*
   (`resolution_eval.rs` snapshots and restores them per block), so the move
   needs a scope-aware side table rather than a flat one, but nothing about them
   is a Raku-level binding.
2. **Type/package names and enum values** (210 of 403). This is the group the two
   correctness issues above are already about, so the scoping work and the
   storage move are the same work.
3. **`__mutsu_constant_var::` / `__mutsu_type::` markers** (33 of 403). Same
   character as group 1.

## 3. Invariants

Whatever the storage, these must continue to hold:

- **Lexical shadowing.** An inner `my` shadows an outer binding of the same name,
  and a block's declarations do not outlive it.
- **Rakudo visibility.** A transitively-`use`d module's names are not visible in
  the importer (#7555, #7787, ADR-0081); a directly-`use`d module's *exported*
  names are.
- **Non-local return targeting.** Whatever replaces
  `data.env.contains_key("__mutsu_callable_id")` must answer the same question:
  does this code object name a routine a `return` can target.
- **Closure capture semantics.** A closure sees the bindings in scope at its
  creation, including later mutations through shared cells (ADR-0025,
  ADR-0039) — the storage move must not turn a shared cell into a snapshot.
- **Thread-clone isolation.** A spawned thread sees the parent's declarations and
  its own do not leak back (#7796, `t/thread-clone-program-table-isolation.t`).

## 4. Acceptance

- `env_deep_copy_entries` (the `MUTSU_VM_STATS` counter added in #7800) per
  `Promise(supply { whenever … })` falls from 17,453 toward the bare-program
  figure of 749.
- The bare-vs-loaded ratio for identical code (0.21 ms vs 1.17 ms today) closes:
  a program's frame costs should not scale with how many modules it has loaded.
- `make test` and `make roast` stay green, and the battery gate does not regress.
- #7787's and #7555 divergence (b)'s visibility tables match rakudo.

## 5. Alternatives rejected

- **Reorder the `clone`/`insert` pairs.** ~18% of the copy cost (~0.09 ms) and
  it changes `&?BLOCK()`'s `return` semantics; see §1.3.
- **Make frame envs scoped overlays everywhere** (the `Env::scoped_child` shape
  ADR-0039 and #7800 use for speculative windows). Sound for a window that is
  discarded, but `Env::iter`/`keys`/`values`/`len` are overlay-only, so any
  long-lived capture — a closure's env, `&?BLOCK`, a thread clone — must
  `flattened()` first, which reintroduces the O(env) copy at exactly the sites
  that matter. It would become viable *after* this ADR, not instead of it.
- **A persistent (HAMT) map for `Env`,** making clone-then-insert O(log n). It
  treats the symptom, keeps the symbol tables in the frame, and leaves the two
  correctness divergences untouched.
- **Do nothing.** Defensible on the perf axis alone — the five merged fixes under
  #7667 already took a HTTP/2 HEADERS frame from 23.4 ms to 10.3 ms — but the
  correctness divergences are filed and open regardless, and they need this same
  work.

## 6. What this does *not* claim

It does **not** claim to stabilize `cro-http`'s `t/http2-request-parser.rakutest`
under `MUTSU_REAL_TEST=1`, which is what #7667 was opened for. That test loses a
race whose losing side (`DATA frame → body-blob → one ok`) costs ~3.1 ms against
a winning side of ~0.25 ms, and needs to come under about 1 ms. Removing *every*
env deep copy on that path is worth about 0.5 ms. This ADR is worth doing for the
correctness divergences and for the general "cost scales with program size"
property; the race needs that and more.

**And that ~1 ms budget is not mutsu's to meet.** The race is a defect in the
test harness — it calls `ok` from `start` blocks, and `Test` is not thread-safe —
reported upstream as
[croservices/cro-http#217](https://github.com/croservices/cro-http/pull/217),
which has each request record its check results into a `Promise` and reports them
from the main thread in request order. With that in, there is no race for an
interpreter to win: the bound becomes the test's own `Promise.in(5)` timeout,
which mutsu clears by two orders of magnitude. So **do not start this campaign,
or any other, on the premise that a HTTP/2 body path must get 3x faster.** Its
justification is §1.4 and §1.2 — the correctness divergences and the fact that
identical code costs 5x more once modules are loaded.

## 7. Implementation status

Tracked by [#7817](https://github.com/tokuhirom/mutsu/issues/7817). The
measurements in §1 are from `MUTSU_VM_STATS`, a `#[track_caller]` dump in
`Env::cow_mut`, and `callgrind`, all reproducible from a plain
`Promise(supply { whenever … })` loop with and without
`use Cro::HTTP2::RequestParser`.

### 7.1 Re-measured before the first slice (2026-10-02)

The composition had moved since §1 (the enum-key namespace of #7914 and the
`MetaNs` key funnel of #8087 had landed), but not the conclusion. A deep-copied
frame env of the loaded program held **824** entries:

| kind | count |
| --- | --- |
| qualified names — type/package objects | 244 |
| qualified names — enum values (`E::K`, `Pkg::E::K`, `Pkg::K`) | 204 |
| `__mutsu_callable_id::Pkg::name` markers | **197** |
| `__mutsu_enum_bare_*` keys | 48 |
| bare lowercase subs/terms | 42 |
| `__mutsu_constant_var::` markers | 35 |
| other `__mutsu_*` markers, sigilled names, constants | 54 |

Per loop iteration (20 iterations minus 0, so module loading is excluded) the
loaded program deep-copied **24,293** entries in 36 copies; the bare program
248 entries in 35 copies.

### 7.2 Slice 1 — group 1 for a module's top-level routines

A routine a loaded module's mainline registers **directly** — at the routine
and block-scope depths the mainline started at — records its registration
clone id in a per-interpreter table (`Interpreter::toplevel_callable_ids`,
`runtime/toplevel_callable_ids.rs`) instead of the importing frame's env. A
module's mainline runs once per process, so that id is fixed for the program's
life and has no lexical extent to track. Every other registration (a sub
declared inside a routine, a block or a loop body, and everything the main
program and `EVAL` declare) keeps its env marker, so the lexical behaviour §2
calls out — a fresh id per clone, a block restoring the enclosing marker — is
untouched. Readers go through `Interpreter::registration_callable_id`: env
first, so a lexical registration shadows, then the table. A thread clone shares
the table copy-on-write like the other program tables (#7796).

Result on the same program: the 197 markers leave the frame env, and the
per-iteration deep-copy volume falls from 24,293 to **18,955** entries (−22%).
The non-local `return` test of §3 is unaffected: it reads the plain
`__mutsu_callable_id` key a call frame sets from the id it resolves, which
still lands in the callee's own frame.

### 7.3 Slice 2 — group 2 for a module's top-level declarations

Re-measured on the same program before the slice (2026-10-03, other work had
landed since §7.2): a deep-copied frame env held ~630 entries, **456** of them
package-qualified names, and one loop iteration deep-copied **10,057** entries
(the bare program: 131).

The two kinds in that group are handled differently
(`runtime/toplevel_package_symbols.rs`), both only for a declaration a module's
mainline makes directly, by the same depth rule as §7.2:

- A **class, role or subset** bound under its own qualified name whose storage
  name is that same name is no longer bound in the frame env at all. The
  binding only repeated what the type registry answers: bareword resolution
  falls back to `has_type`, `::('…')` to the registry, and both apply the #7797
  visibility gate first. A `my` type (mangled storage name), a short alias and a
  `unit module`'s own package binding are unchanged.
- An **enum value**'s three qualified spellings (`E::K`, `Pkg::E::K`,
  `Pkg::K`) go to a per-interpreter table, `Interpreter::module_toplevel.package_symbols`,
  that frames neither clone nor capture. Bareword lookup, indirect lookup, the
  package stash and the `need` hiding scans consult it after the env; a thread
  clone shares it copy-on-write.

Result: a deep-copied frame env falls to ~220 entries, and the per-iteration
deep-copy volume from **10,057** to **3,401** entries (−66%).

**Scope correction (#11249).** The original slice put all three enum member
spellings in the process-wide table. Rakudo makes an unexported `E::K` visible
only in the declaring unit module, while `Pkg::E::K` and `Pkg::K` remain package
symbols. The short private spelling now lives in `module_scope_lexicals` keyed
by its declaring package; qualified readers consult that scope before the
process-wide table. Exported members keep their imported short spelling.

What remains of the env's non-lexical content after this slice: the
`__mutsu_enum_bare_*` keys of imported enum values (44), group 3's
`__mutsu_constant_var::` / `__mutsu_type::` markers (45), the qualified names
of packages a `unit module` declares and of types declared below a module's top
level (46), and group 1 for the main program's own top-level routines.

### 7.4 Slice 3 — group 3, the `constant` and type markers

Re-measured before the slice (2026-10-03) on a `Promise(supply { whenever
Supply.from-list(1, 2, 3) { emit $_ } })` loop under
`use Cro::HTTP2::RequestParser`, debug build, entries per loop iteration taken
as the difference between a 20-iteration and a 0-iteration run: **6,405**
entries in 30 deep copies. Of those, 1,050 were `__mutsu_constant_var::`
markers (35 per copied env) and 300 `__mutsu_type::` markers (10 per env).
(The loop body differs from §7.3's, so its totals are not comparable with
§7.3's figures; the per-env composition is.)

The two kinds are handled differently (`runtime/toplevel_markers.rs`):

- A **`constant` marker** a module's mainline writes directly (the depth rule
  of §7.2) goes to a per-interpreter table keyed by the declaring package,
  `Interpreter::module_toplevel.constant_markers`. Its two readers — regex
  `$name` interpolation and the EVAL parser's declared-term set — consult the
  env first and then the running package's chain, the same lookup a
  package's own `constant` values already use. So a `unit module`'s markers
  are visible to its own routines and not to the importer (whose view of the
  values #7787 had already cut), and a package-less module file's land under
  GLOBAL, where its classes' methods and the importer still see them, as they
  still see its leaked values. A later `my $name` that must hide a table
  marker for its own scope writes `False` in the env instead of removing the
  key.
- A **type marker** belongs with its binding. A `unit module`'s file-scope
  constants, `our` variables and `my` variables lose their env binding once
  the body has run, but their type markers stayed behind, orphaned
  (`our int32 constant VERSION` in `OpenSSL::Version`, `my int $be16` in
  `CBOR::Simple`). The load now snapshots the importer's markers under those
  names before the body and restores them afterwards, exactly as it restores
  the bindings.

Result: no `__mutsu_constant_var::` marker and one `__mutsu_type::` marker
(`current-id`, declared inside a class body) remain in a copied frame env, and
the per-iteration deep-copy volume falls from **6,405** to **5,085** entries
(−21%). `t/modules/module-toplevel-markers-off-frame-env.t` pins that every
reader still answers as rakudo does.

What remains of the env's non-lexical content: the `__mutsu_enum_bare_*` keys
of imported enum values (44 per env, now the largest group), the qualified
names of packages a `unit module` declares and of types declared below a
module's top level, the companion markers of a block-form `module X { my $a
:= … }` body's own bindings (`__mutsu_scalar_bind_no_container::` /
`__mutsu_bound_decont::`, 20 per env, from `JSON::Fast`), and group 1 for the
main program's own top-level routines.

### 7.5 Slice 4 — a module's top-level enum keys

Re-measured before the slice (2026-10-04) on §7.4's program and method: **5,085**
entries per loop iteration in 30 deep copies, each copied frame env carrying
44 `__mutsu_enum_bare_*` keys — the largest remaining group. They are the bare
keys of enums the loaded modules' mainlines declare directly: package-less
files' `enum Settings <…>` (`Cro::HTTP2::Frame`), `enum Pkg::E <…>`
declarations, and a class body's `my enum State <…>`
(`Cro::HTTP::RawBodyParser::Chunked`).

Such a key (the depth rule of §7.2) now goes to a per-interpreter table keyed
by the declaring package, `Interpreter::module_toplevel.enum_keys`
(`runtime/enum_bare_names.rs`), with `GLOBAL` as the owner of a package-less
file's keys. `enum_bare_value` — the one reader every bareword route already
goes through — asks the env first, then the running package's chain, then
`GLOBAL`. So:

- a package-less module's keys stay visible everywhere, the importer
  included, as in rakudo;
- a package's keys (a `unit module`, a `module M { … }` block, a class body)
  are visible to that package's code, including closures it creates, which
  run with it as their lexical package, and not to the importer. This fixes
  a divergence: a class body's `my enum` keys used to leak into the loading
  scope (`::('AwaitingLength')` answered from the importer), because the
  class-body exit leaves `my enum` keys alone for the sake of one declared
  inside a method;
- an import, or the program's own enum, is an env key and shadows a table
  entry.

Result: no `__mutsu_enum_bare_*` key remains in a copied frame env, and the
per-iteration deep-copy volume falls from **5,085** to **3,765** entries
(−26%). `t/modules/module-toplevel-enum-keys-off-frame-env.t` pins each
key's visibility against rakudo's.

What remains of the env's non-lexical content: the qualified names of
packages a `unit module` declares and of types declared below a module's top
level (40 per env), the companion markers of a block-form `module X { my $a
:= … }` body's own bindings (`__mutsu_scalar_bind_no_container::` /
`__mutsu_bound_decont::`, 20 per env, from `JSON::Fast`), and group 1 for the
main program's own top-level routines.

### 7.6 Slice 5 — a module's qualified package declarations

Re-measured before the slice (2026-10-04, §7.5's program and method): **3,765**
entries per iteration. Of the ~40 qualified names a copied frame env still
carried, 17 were the self-bindings of qualified packages the loaded modules
declare: `unit module JSON::Fast`, `OpenSSL::Ctx` and its siblings,
`package Example::A { }` blocks, and the `EXPORT::<name>` packages a module's
`sub EXPORT` machinery declares.

`OpCode::RegisterPackage` now skips that binding under the same rule §7.3 applies
to a class: the name is qualified, a module's mainline is declaring it directly,
and the env holds no binding the new one would replace
(`qualified_identity_binding_is_redundant`). A bare `unit module Foo` keeps its
binding, as does every package the main program or a nested block declares.

What the binding answered is now answered by the package's kind record,
`Registry::package_kinds`, which (unlike `chain_declared_packages`) outlives the
`use` that declared it:

- indirect lookup (`::('Example::C')`) accepts a qualified name that has a kind
  record, next to the class/role/enum registry check it already made;
- `is_declared_package` and the export-value lookup (`module Inner is export`,
  `unit module P::Q::Fac is export`) ask the same record
  (`is_qualified_package_decl`, which excludes a `my package`);
- a package stash (`Example::.keys`) lists those packages as members, under the
  same `need`/transitive-hiding and `my`-scope filters as its class loop. This
  also fixes a divergence: without precompilation, `Example::.keys` after
  `use Example::A; use Example::B` used to be empty (rakudo: `A B C`).

Result: the per-iteration deep-copy volume falls from **3,765** to **3,195**
entries (−15%). `t/modules/module-toplevel-package-names-off-frame-env.t` pins
the readers.

What remains of the env's non-lexical content: qualified enum and type names
bound below a module's top level or under a name that differs from their storage
name (`my` types with mangled storage names, `X::Malformed` aliasing
`CBOR::Simple::X::Malformed`), qualified `our constant`s
(`OpenSSL::Version::VERSION`), qualified `&` code bindings, the
`__mutsu_scalar_bind_no_container::` / `__mutsu_bound_decont::` companion markers
of a block-form `module X { my $a := … }` body (20 per env, from `JSON::Fast`),
and group 1 for the main program's own top-level routines.

### 7.7 Slice 6 — a module block's `:=` binding markers

Re-measured before the slice (§7.6's program and method): **3,195** entries
per iteration. Each copied frame env carried 20 markers from
`module JSON::Fast { … }`, one `__mutsu_bound_decont::<name>` and one
`__mutsu_scalar_bind_no_container::<name>` per file-scope `my $x := …`
(`$hexdigits`, `$escapees`, `$ws`, …).

They were not module top-level declarations at all. A `package`/`module`
block's exit (`exec_package_scope_op`) drops the bare keys the block
introduced and carries every key containing `::` out, on the assumption that
such a key is package-qualified. These markers name a bare binding but their
key has a `::`, so they left the block while the binding they describe did
not. The exit now drops a `BoundDecont` / `ScalarBindNoContainer` marker that
the block introduced and whose subject is a bare name. A marker the outer
scope already had is kept as before.

No reader is affected. The package's routines never read these markers
through the caller's env, which is what the leaked copy offered: a routine
closing over a `:=`-bound list iterates it as one item whether the marker is
present or not. That is a separate, pre-existing bug, filed as #11853.

Result: the per-iteration deep-copy volume falls from **3,195** to **2,595**
entries (−19%). `t/modules/module-block-bind-markers-off-frame-env.t` pins the
bindings' behaviour.

What remains of the env's non-lexical content is §7.6's list minus these
markers. That is qualified names bound below a module's top level or under an
alias, qualified `our constant`s, qualified `&` code bindings, and group 1 for
the main program's own top-level routines.

### 7.8 Slice 7 — qualified `our constant` package symbols

A module's mainline publishes a qualified `our constant` through
`SetGlobalRaw`. When the importing env has no binding under that qualified
name, the store now uses `module_toplevel.package_symbols` instead. The
existing qualified readers and package stash already consult that table after
the env; an existing env binding continues to win. The bare binding used by
the module itself, declarations below its top level, and the main program's
declarations keep their former storage.

On a current-main debug build, a 20-iteration closure-call probe after loading
`FrameEnvConstants` deep-copied 31 env entries before this change and 30 after
it. Both runs made 41 deep copies. This small fixture exposes only one fewer
copied entry; it is a storage check, not a wall-clock speed claim.
`t/modules/module-qualified-our-constants-off-env.t` checks the direct and
indirect names, aggregate constants, the module's own reads and its stash
against Rakudo.

The remaining qualified env entries are bindings below a module's top level
or under an alias, and qualified `&` code bindings; group 1 for the main
program's own top-level routines also remains.

### 7.9 Slice 8 — qualified `our &` needs one mutable binding

The qualified `&` remainder cannot be retired by moving only its top-level
`SetGlobal` write to `ModuleToplevel::package_symbols`. A trial with
`unit module M; our &answer = { 42 }` reached that table, but the final
`set_shared_var` call also inserted `&M::answer` into the frame env. A paired
20-iteration closure-call probe reported the same 26 deep-copied env entries
before and after the trial. The existing shared-variable mirror would have
kept the qualified key in every copied frame.

There is a correctness prerequisite as well. After `use M`, assigning
`&M::answer = { 43 }` makes the qualified call return 43 but a routine in M
that calls its bare `&answer` still returns 42; Rakudo returns 43 through both
names. When the assignment runs in `start`, Rakudo again returns 43 through
both names, while mutsu returns 42 through both. [#11913](https://github.com/tokuhirom/mutsu/issues/11913)
records both reductions. Moving the qualified value alone would preserve this
split binding and could worsen it across thread clones.

The next implementation must make the package-qualified name and the module's
bare `our &` alias address one mutable binding, and route thread writes through
the same binding. The shared-variable publication path must be able to update
that binding without re-inserting the qualified key into `Env`. Pin direct,
indirect, stash, module-owned and thread reads against Rakudo, then re-measure
the frame-env copy count. No interpreter change from this trial was retained.

### 7.10 Slice 9 — a top-level `constant` leaves the env for real

§7.8's store to `ModuleToplevel::package_symbols` did not keep the qualified
name out of the frame env: the same `SetGlobal` handler then called
`set_shared_var`, which inserts the key into the env as part of publishing it
to the cross-thread store. The §7.8 probe's single saved entry came from that
interplay, not from the move. This is the same re-insertion §7.9 found for
`our &`.

A `constant` binding never changes after its declaration, so it needs neither
the env nor the shared store. `SetGlobalRaw` now carries a `constant` flag (the
other raw store, an `our` aggregate that already holds its coerced container,
is a mutable variable and keeps the old path), and a constant that a module's
mainline publishes under a qualified name goes to the package-symbol table
only. That covers `constant &name`, which a module publishes as
`&Pkg::name`: the qualified code-variable read (`resolve_code_var`) and the
qualified call fallback now consult the table after the env, like the other
qualified readers.

Measured on ADR-12529's FunctionalParsers probe, whose module declares twelve
`constant &alt is export = &alternatives`-style shortcuts: those names left
every closure capture's shared layer, and by-name chain walks fell 19%.
`t/modules/module-qualified-code-constants-off-env.t` pins the direct,
qualified, indirect, stash, module-owned, closure and thread reads against
Rakudo.
