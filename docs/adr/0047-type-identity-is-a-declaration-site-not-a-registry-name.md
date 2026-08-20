# ADR-0047: A type's identity is its declaration site, not its current registry name — retiring `subtest`'s registry rollback

- Status: Partially adopted — P1 and P2 landed (PR #6757); P3/P4 not started
- Date: 2026-08-20
- Supersedes: nothing
- Related: [ADR-0024](0024-mainline-lexicals-for-named-subs.md) (name-vs-lexical resolution),
  [ADR-0039](0039-container-lexicals-resolve-lexically.md) (retiring by-name container
  resolution — the same "resolve lexically, not by name" move for `@`/`%`),
  [ADR-0042](0042-type-constraints-belong-to-the-container-not-to-a-name.md)
  (retiring the `var_type_constraints` side table, which `subtest` also snapshots)

## Context

### What a type object is in mutsu

A Raku type object is represented as `Value::Package(Symbol)` — a **bare name**.
An instance is `Value::Instance { class_name: Symbol, .. }` — also a bare name.
Neither carries a handle to the thing that defines its behaviour. Every
operation that needs the definition (`.new`, attribute defaults, `BUILD`/`TWEAK`,
MRO walking, role composition, `trusts`, private-method validation) resolves the
name against a single, process-global, mutable `Registry` — `registry().classes`,
`registry().roles`, `registry().subsets`, and roughly forty sibling tables
(`src/runtime/registry.rs`).

So **type identity is defined as "whatever `registry.classes[name]` holds right
now"**. That is the root fact this ADR is about. Every consequence below follows
from it mechanically.

### Three mechanisms that scope declaration names, all name-keyed, all wrong

Raku's rule is simple and has two independent halves:

1. A `my class C { }` binds the **name** `C` in the enclosing lexical scope, and
   that binding disappears at scope exit.
2. The **type itself** is immortal. A type object that escaped the scope (assigned
   to an outer variable, closed over, pushed into a data structure, returned) stays
   fully functional forever — `.new`, attribute defaults, methods, everything.

mutsu currently implements half 1 three different ways, and each one damages half 2:

**(a) `subtest`'s wholesale registry rollback**
(`src/runtime/test_functions/tap_subtest.rs`, `snapshot_subtest_decls` /
`restore_subtest_decls`). Before running the body it clones nine registry tables
plus `loaded_modules`, `type_metadata` and `var_type_constraints`; afterwards it
assigns them straight back. Introduced in #399 (2026-02) as three inline
save/restore sequences, consolidated into `SubtestDeclSnapshot` by #5379.

**(b) `pop_lexical_class_scope`**
(`src/runtime/runtime_encoding.rs:146`). On block exit it suppresses the declared
names (`suppressed_names`) and **releases** the declaration site's claim in
`lexical_class_sites`, so a later same-named lexical class reuses the bare
registry key.

**(c) `decl_id` storage-name mangling**
(`src/vm/vm_typedecl_ops.rs:203`). When a *different* declaration site reuses a
name while the earlier site's claim is still live, the new class is stored under
`Name\0<decl_id>` so it cannot clobber the first. This is the only one of the
three that is actually correct — and (b) deliberately disarms it.

### Verified symptoms (all on `main` at 227e38e4f, debug build)

**S1 — a type object that escapes a subtest becomes half-dead.**

```raku
use Test;
plan 1;
my $captured-type;
subtest {
    my class Upper { method go() { "hi" } }
    $captured-type = Upper;
    ok 1, 'declared';
};
say $captured-type.^name;      # Upper       (works)
say $captured-type.new.go();   # X::Method::NotFound: ... new on Upper
```

raku prints `Upper` / `hi`. mutsu dies on `.new`.

Note *which* things survive. `SubtestDeclSnapshot` covers only nine of the
registry's ~forty tables — `method_entries`, `class_composed_roles`,
`class_attribute_defaults`, `enum_types`, `package_kinds`, `lexical_classes` and
the rest are **not** rolled back. So the restore does not remove the type, it
**tears** it: methods and `.^name` still resolve, the `ClassDef` does not. An
instance constructed *inside* the subtest keeps working after the restore; the
type object it came from cannot make another one. A subset declared in a subtest
loses `.ACCEPTS` the same way. Half-registered is strictly worse than either
"kept" or "removed", and no amount of adding fields to the snapshot fixes it
(see "Rejected alternatives").

**S2 — two same-named lexical classes silently retarget each other's instances.**

```raku
my ($a, $b);
{ my class C { method go() { "first"  } }; $a = C.new; }
{ my class C { method go() { "second" } }; $b = C.new; }
say $a.go();   # raku: first     mutsu: second
say $b.go();   # raku: second    mutsu: second
```

The first block's `pop_lexical_class_scope` released the `C` claim, so the second
declaration took the bare key and overwrote `registry.classes["C"]`. `$a` is a
`Value::Instance { class_name: "C" }`, so it now dispatches into the *second*
class. This has nothing to do with `subtest` — it is mechanism (b) alone — but
it reproduces identically with two sibling subtests, and unlike S1 it is
**silent**: a wrong answer, not an exception.

**S3 — an inner declaration steals the outer name permanently.**

```raku
my class Foo { method go() { "outer" } }
my $o = Foo.new;
{ my class Foo { method go() { "inner" } } }
say $o.go();       # raku: outer   mutsu: outer   (mangling did its job)
say Foo.new.go();  # raku: outer   mutsu: inner
```

Here mangling (c) *did* fire — `$o` still reaches the outer class, and `.^name`
correctly reports the unmangled `Foo` — but the env binding of the name `Foo` was
overwritten by the inner declaration and never restored on block exit. So half 2
was preserved and half 1 was not, the exact mirror of S1.

**S4 — the same fossil blocks a measured perf win.** PR #6499 dispatched the
subtest body through the compiled closure path instead of the AST carrier
(which calls a fresh `Compiler::compile()` on *every* subtest invocation). It was
reverted because it also changed which declarations were inside the
snapshot/restore window. As long as class lifetime depends on a snapshot window,
*any* change to how the subtest body is invoked is a correctness risk. (The
compiled path has a second, independent regression that is **not** about class
registration — see `todo/deep/subtest-compiled-dispatch-async-middleware-regression.md`.
This ADR removes the registry-lifetime coupling; it does not claim to fix that one.)

### Why this is architectural, not a patch

`restore_subtest_decls` cannot be fixed in place. Its job — "undo the declarations
this block made" — is only well-posed if you can decide which of them are still
reachable, and you cannot: a type object is an ordinary `Value` that can be
stashed anywhere, closed over, or handed to another thread. Any predicate
approximating "was it observed outside?" is a heuristic that fails silently.

The real defect is that mutsu asks a *name* to carry an *identity*. Everything
above is that one confusion, seen from four angles.

## Decision

**A declaration site owns a stable, unique registry key for the life of the
process. A name is a lexical binding to that key, and nothing else. Scope exit
removes bindings; it never removes definitions.**

Concretely: keep `Value::Package(Symbol)`, but make the Symbol a *site key*
rather than a source name for lexically-scoped declarations, and make every
name→key step go through the lexical environment.

### D1 — Site keys become unconditional for lexical declarations

`vm_typedecl_ops.rs` mangles to `Name\0<decl_id>` only when it detects a live
collision. Make it unconditional: **every** `my`-scoped `class`/`role`/`subset`/
`grammar`/`enum` declared at a site with `decl_id != 0` registers under
`Name\0<decl_id>`, and the declaring scope binds the short name in `env` to
`Value::Package("Name\0<decl_id>")`.

Two distinct sites then never share a registry key, so S2 and S3 cannot happen by
construction. `lexical_class_sites` / `lexical_class_owner_scopes` — the
claim/release bookkeeping that exists *only* to arbitrate bare-key sharing —
becomes dead and is deleted, and with it the release step in
`pop_lexical_class_scope` that causes S2.

The presentation layer already handles this: `.^name`, `.WHAT.gist` and
`restore_nested_type_short_names` all strip the `\0` suffix today (verified in
S3, where mangling was live and `.^name` still printed `Foo`). Making mangling
unconditional widens an existing path rather than opening a new one — which is
the main reason to prefer it to the theoretically cleaner handle route below.

### D2 — Scope exit restores name bindings, and only name bindings

`pop_lexical_class_scope` keeps its `suppressed_names` half and additionally
**restores** the previous env binding for each name the scope shadowed (the same
save-previous/restore-on-exit shape `persist_class_body_statics` and
`restore_nested_type_short_names` already use in
`src/runtime/registration_class_body_exit.rs`). That fixes S3's second line. No
registry table is touched at scope exit, ever.

### D3 — `subtest` stops rolling back the registry

Delete `SubtestDeclSnapshot`, `snapshot_subtest_decls` and
`restore_subtest_decls`. With D1 in place, a subtest body's declarations cannot
collide with anything outside it, and its *names* are already scoped by the
existing `saved_env` / `merged_env` dance in `test_fn_subtest` plus D2. This is
what makes S1 go away, and it removes the torn-registry class of bug entirely
rather than enlarging the snapshot.

`subtest` then stops being the one construct in the language with bespoke
declaration semantics — it becomes an ordinary block, which is what Raku says it
is.

Three snapshot members need individual dispositions:

- **`loaded_modules`** — added by #5379 *because of* the rollback: a module first
  `use`d inside a subtest had its declarations erased yet still counted as
  loaded, so the next `use` short-circuited and installed nothing. With the
  rollback gone the declarations survive, so the workaround must go too —
  re-running a module's mainline once per subtest was never right. `t/subtest-module-reuse.t`
  is the pin; it must keep passing *without* the rollback, and if it does not, that
  is a genuine `use`-idempotence bug to fix on its own terms, not a reason to keep
  the snapshot.
- **`var_type_constraints`** — ADR-0042 is retiring this side table into the
  container. Do not re-plumb it here; sequence D3 after ADR-0042 slice 1, or drop
  it from the snapshot as part of that work.
- **`type_metadata`** — already restored with a merge (`or_insert`) rather than a
  wholesale assignment, i.e. it is already half-admitting that wholesale rollback
  is wrong. It goes with the rest.

### D4 — Out-of-scope lexical types stay in the registry

After D1–D3 nothing is ever removed from `registry.classes`. A `my class` in a
loop body or a hot subtest therefore accumulates one entry per *site*, not per
execution (the `decl_id` is parse-time and stable across re-executions — that
property is already relied on and must be preserved). This is a bounded leak
proportional to program text, and it is the same leak mutsu already has for every
non-subtest lexical class (`registry.lexical_classes`' doc comment: "only still
present because mutsu has no scope-exit cleanup for `registry().classes`").

Making these entries collectable requires the handle route (below) and is
explicitly **not** part of this ADR.

## Phasing

Each phase is independently landable and independently verifiable.

- **P1 (D1) — LANDED (PR #6757).** Unconditional site keys; deleted
  `lexical_class_sites`/`lexical_class_owner_scopes` arbitration. Pins: S2 and
  S3 as `t/lexical-class-sibling-identity.t`. Extended to `grammar` as well:
  `my grammar`/`our grammar` never actually set `is_lexical` before this PR
  (`grammar_decl` hardcoded `is_lexical: false` regardless of the `my`/`our`
  prefix), so `grammar_decl` was split into a plain (package-scoped) entry
  point and `grammar_decl_my` that threads `is_lexical` through, mirroring
  `class_decl_body`. A narrower `lexical_class_pending`/
  `lexical_class_pending_scopes` mechanism (scoped strictly to
  currently-open scopes) was added so a stub (`my class C { ... }`) and its
  own later full definition in the SAME open scope — two separate
  `decl_id`s, since the parser assigns a fresh id per `Stmt::ClassDecl` node
  even for a textually-adjacent stub+definition pair — still share one
  registry entry, without reintroducing S2/S3's cross-scope reach.
  `role`/`subset` were investigated but found to have NO existing
  `decl_id`/`is_lexical` mangling infrastructure at all (unlike `class`/
  `grammar`, which share one code path) — extending them is a separate,
  larger follow-up, not part of this landing.
  Making mangling unconditional (rather than collision-only) surfaced
  several sites that assumed a lexical class's registry key equals its
  source-written name; each was fixed to resolve through the lexical env or
  demangle for display (bareword/call-position type resolution, qualified
  method dispatch, private-method owner resolution, role `is`/`hides`
  parent resolution, variable tie traits, and several exception/error
  message constructors) — see PR #6757's description for the full list.
- **P2 (D2) — LANDED (PR #6757).** Name-binding restore at scope exit.
  Implemented by enrolling a lexical class's declared name in
  `block_declared_vars` (the same set an ordinary `my $x` joins), so the
  EXISTING general block-exit restore machinery in `vm_misc_scope.rs`
  reverts the bare-name env binding on scope exit exactly like a shadowed
  lexical variable — no bespoke restore mechanism needed. Pin: S3's second
  line (part of `t/lexical-class-sibling-identity.t`).
- **P3 (D3) — NOT STARTED.** Delete the subtest registry rollback. Pins: S1 as
  `t/subtest-escaped-type-stays-constructible.t`; `t/subtest-module-reuse.t` must
  still pass (verified still passing after P1+P2, unaffected by this slice).
  Gate on the `scripts/battery-testsuite.sh` whitelist, which is what
  caught #6499.
- **P4 (follow-up, not scheduled here)** — with lifetime decoupled from the
  dispatch path, re-attempt #6499's compiled-first `subtest_call_block`. Note this
  is *necessary but not sufficient*: the compiled path has a separate, un-root-caused
  async regression (see the todo file named in S4).

P1 and P2 were landed even though P3 is deferred: S2 is a silent wrong answer
and does not involve `subtest` at all.

## Rejected alternatives

**"Only roll back entries never observed outside the subtest"** (the original
todo file's own suggestion). Rejected: reachability of a `Value` is not decidable
at the restore point — it can be in a closure, an array, a `Promise`, or another
thread. And it does nothing for S2/S3, which are the silent failures.

**"Snapshot all ~forty registry tables instead of nine."** Rejected: it converts a
torn type into a fully deleted one, which is *also* wrong (raku keeps it alive),
and it makes every future registry field a correctness landmine — the exact trap
#5379 documented and then re-created.

**"Make `Value::Package` carry `Arc<ClassDef>` instead of a name."** This is the
theoretically right end state: identity becomes a pointer, names become pure
bindings, and D4's leak becomes GC-able. Rejected *for now* on blast radius —
every `class_name: Symbol` site, `.^name`/`WHO`/`HOW`, MRO-by-name, role
composition by name, `trusts`, serialization and the `Instance` representation
would all have to move together. D1's site key is the same idea with a string
key, reusing machinery that already exists and already demangles correctly.
Record it as the successor decision; do not start it as part of this ADR.

## Consequences

- `subtest` loses its bespoke declaration semantics and becomes a plain block.
  One fewer place where "what does `my class` mean" has a special answer.
- Three name-scoping mechanisms collapse to one (bind in `env`, restore on exit).
  `lexical_class_sites`, `lexical_class_owner_scopes` and `SubtestDeclSnapshot`
  are all deleted.
- Registry keys stop being human-readable source names for lexical declarations.
  Anything that greps `registry.classes` by literal name must go through the same
  demangling helper the presentation layer uses. This is the main migration hazard
  and P1's review should look for it specifically.
- The per-call `Compiler::compile()` in the subtest AST carrier becomes safe to
  remove (P4), which is where #6499's measured win comes back.
