# ADR-0029: Built-in `X::` exception ancestry is role membership, not a single parent — register it through the existing composed-role path

- **Status**: Proposed (Slices 1-3 implemented 2026-08-17/18; Slice 4 deferred
  — see "Implementation status" at the end)
- **Date**: 2026-08-17
- **Context ticket**:
  [`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`](../../todo/deep/exception-class-hierarchy-is-mostly-unregistered.md)
  (filed 2026-08-03; its headline "77 registered / 124 missing" is re-measured below)
- **Related**: [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md)
  (its E2b slice added the last 25 `register_x` rows and is where the
  `dispatch_owner_chain` MRO gap was first surfaced);
  [`todo/deep/vendor-real-test-module.md`](../../todo/deep/vendor-real-test-module.md)
  (the sweep in which the gap is observable, since rakudo's real `Test`
  `throws-like` checks the *type*, not the class name)

## Context

This is **architecture-correctness work, not progress-by-metric work.** Per
CLAUDE.md's Gain/Risk section, the gain here is moving mutsu toward the
*correct* type-system shape — a mechanism that can express what rakudo's
exception hierarchy actually is. It is explicitly **not** a roast-clearing
campaign: the source ticket's own probe found that only 2 of 9 remaining
regressions in one sweep were caused by an unregistered exception class, and
this ADR deliberately promises no test-count win. What it promises is that
`.^mro`, `.^roles`, and `~~` stop disagreeing with rakudo about types mutsu
itself raises.

### 1. The mechanism cannot express the shape, so the rows we already have are mostly wrong

`src/runtime/runtime_init.rs` seeds built-in exception types through a closure

```rust
let mut register_x = |name: &str, parent: &str| { ... };
```

which takes **exactly one parent name**, synthesises an MRO by walking that
parent's chain, and writes `parents: vec![parent]`. There are **107** such
calls today (`grep -c 'register_x(' src/runtime/runtime_init.rs`), up from the
ticket's 77 — the ADR-0019 E2b backfill (`runtime_init.rs:1767-1811`) added 25
of them.

Rakudo's `X::` hierarchy is not shaped like that. Measured against the real
`raku` on this machine (2026-08-17), over the **303** `X::` names that appear
anywhere in `t/` + `roast/` and are real rakudo `Exception` subclasses:

| shape | count | share |
| --- | --- | --- |
| does at least one role | 178 | 59% |
| has a real intermediate superclass (mro deeper than `X Exception Any Mu`) | 33 | 11% |
| distinct role names needed to cover *all* of the role half | **14** | — |

The role vocabulary is tiny and heavily concentrated: `X::Comp` alone is done
by 135 of the 303, `X::Syntax` by 69, `X::IO`/`X::OS` by 22 each; the whole
remaining tail is `X::Trait` (7), `X::Proc::Async` (7), `X::BadType`,
`X::Temporal` (3 each), `X::MOP`, `X::Encoding`, `X::Pod` (2 each),
`X::Wrapper`, `X::RoleApplier`, `X::RoleApplier::Method` (1 each).

Because the mechanism has only a parent slot, every one of those role
memberships that mutsu *does* model has been modelled as inheritance. The
result, measured on the **105** corpus classes mutsu currently registers and
can construct:

| | count |
| --- | --- |
| `.^mro` **and** `.^roles` byte-identical to rakudo | 47 |
| wrong `.^mro` — 36 with a **false superclass**, 4 missing one, 3 both | 43 |
| wrong `.^roles` — **all 52 report the empty list** | 52 |

So slightly under half of what is already registered is registered wrong, and
mutsu reports `()` for `.^roles` on *every* `X::` class without exception. This
is the point of the ADR: adding 120 more rows through the current mechanism
would add 120 more wrong rows.

Concrete pairs (mutsu on the left, `raku` on the right, both run today):

```
X::Syntax::Confused.^mro
  mutsu:  (X::Syntax::Confused X::Syntax X::Comp Exception Any Mu)
  raku:   (X::Syntax::Confused Exception Any Mu)          roles: (X::Syntax X::Comp)

X::Comp::AdHoc.^mro
  mutsu:  (X::Comp::AdHoc X::Comp X::AdHoc Exception Any Mu)   roles: ()
  raku:   (X::Comp::AdHoc X::AdHoc Exception Any Mu)           roles: (X::Comp)

X::Comp::NYI.^mro
  mutsu:  (X::Comp::NYI X::Comp Exception Any Mu)
  raku:   (X::Comp::NYI X::NYI Exception Any Mu)          roles: (X::Comp)

X::Comp::FailGoal
  mutsu:  ~~ X::Comp is False; .^mro (X::Comp::FailGoal Exception Any Mu)
  raku:   ~~ X::Comp is True;  .^mro (X::Comp::FailGoal Exception Any Mu)  roles: (X::Comp)
```

### 2. The name-prefix rule is wrong more often than right (ticket's table, re-verified)

The ticket's central warning still holds verbatim; every row below was re-run
against `raku` on 2026-08-17. The rule "parent = the longest registered
`::`-prefix" bakes in inheritance rakudo does not have:

| child | prefix | prefix is a real ancestor? | what it actually is |
| --- | --- | --- | --- |
| `X::IO::Mkdir` | `X::IO` | yes — but as a **role** | roles `X::IO X::OS`; mro `… Exception Any Mu` |
| `X::Syntax::Perl5Var` | `X::Syntax` | yes — as a **role** | roles `X::Syntax X::Comp` |
| `X::Comp::Trait::Unknown` | `X::Comp` | yes — as a **role** | parent `X::Trait::Unknown`; roles `X::Comp X::Trait` |
| `X::Bind::Slice` | `X::Bind` | **no** | mro `X::Bind::Slice Exception Any Mu`; no roles |
| `X::Numeric::DivideByZero` | `X::Numeric` | **no** | `X::Numeric` is a bare `PackageHOW` — not a type at all |
| `X::Attribute::Required` | `X::Attribute` | **no** | `X::Attribute` is a bare package; roles `X::MOP` |
| `X::Placeholder::Block` | `X::Placeholder` | **no** | `X::Placeholder` is a bare package; roles `X::Comp` |
| `X::Syntax::Malformed::Elsif` | `X::Syntax::Malformed` | **no** | roles `X::Syntax X::Comp` |
| `X::Str::Sprintf::Directives::BadType` | `X::Str::Sprintf::Directives` | **no** | prefix is a bare package; no roles |
| `X::Parameter::RW` | `X::Parameter` | **no** | prefix is a bare package; no roles |

Probing the prefixes' metaobjects makes the taxonomy explicit — a `X::`
namespace segment is one of **three** unrelated things, and the name gives no
hint which:

```
X::Comp X::Syntax X::IO X::OS X::MOP X::Trait X::Control  -> ParametricRoleGroupHOW  (role)
X::AdHoc X::Undeclared X::Bind                            -> ClassHOW               (class)
X::Numeric X::Parameter X::Placeholder X::Attribute
X::Str::Sprintf::Directives X::CompUnit X::UnitScope      -> PackageHOW             (namespace only)
```

mutsu today registers `X::Comp` and `X::Syntax` as *classes* — the wrong kind
of thing — which is precisely why 36 of its rows carry a false superclass.

### 3. The `X::Comp::AdHoc` splice — the existing ad-hoc workaround

`runtime_init.rs:1813-1829` already hit this wall once and worked around it by
hand, immediately after the `register_x` block:

```rust
// X::Comp::AdHoc does both X::Comp and X::AdHoc in rakudo ... register_x only
// threads a single parent, so splice X::AdHoc into the MRO here ...
if let Some(def) = classes.get_mut("X::Comp::AdHoc") {
    if !def.parents.iter().any(|p| p == "X::AdHoc") { def.parents.push("X::AdHoc".into()); }
    ... def.mro = mro.into();
}
```

It makes `$e ~~ X::AdHoc` answer `True`, which was the goal, and it is the
obvious thing to generalise. But measured against `raku` it is *half* right and
half actively wrong, and it illustrates the trap:

- `X::AdHoc` genuinely **is** a superclass of `X::Comp::AdHoc` in rakudo
  (`mro: X::Comp::AdHoc X::AdHoc Exception Any Mu`), so splicing it into
  `parents`/`mro` was correct — by luck of which of the two ancestors got the
  bespoke treatment.
- The `register_x("X::Comp::AdHoc", "X::Comp")` call it sits next to is the
  wrong one: `X::Comp` is a **role**, so mutsu's `.^mro` gains a spurious entry
  and its `.^roles` stays empty.

Generalising the splice would therefore propagate the *wrong half* of this
precedent to ~178 more classes: `~~` would answer correctly while `.^mro` and
`.^roles` grew ~178 more disagreements with rakudo. A mechanism that is right
only for `~~` is not the correct architecture; it is the current band-aid,
scaled up.

### 4. The correct mechanism already exists in mutsu, and is already used in this same file

Role membership is **not** modelled through `ClassDef` — `ClassDef`
(`src/runtime/decl_types.rs:19`) has only `parents: Vec<String>` and `mro`, and
no roles field, which is what makes the splice tempting. It is modelled in the
**Registry**:

- `Registry::class_composed_roles: HashMap<String, Vec<String>>` — documented
  in `src/runtime/registry.rs:204` as *"the FLATTENED set (includes roles
  reached transitively …), used for `~~`/role-membership checks"*;
- `Registry::class_direct_composed_roles` — the non-transitive list;
- `Registry::class_does_only_roles` — *"A `does`-composed role provides methods
  but is NOT an MRO entry in Rakudo's `.^mro_unhidden`"*;
- `Registry::role_parents` / `Registry::role_parents_of` /
  `Registry::builtin_role_parents` — role-to-role composition.

`registration_class_body_does.rs` (the real user-facing `does` trait handler)
writes exactly these, via `record_class_composed_roles`. Every consumer already
reads them: `.^roles` (`methods_classhow_parents.rs::collect_roles_for_class`),
`.^parents` / MRO introspection (`methods_classhow_mro.rs`), qualified
`self.Role::meth` dispatch (`methods_qualified.rs`), method-candidate
collection (`dispatch_candidates.rs`), and the smartmatch/type-constraint
checker (`types/type_matching.rs`, whose instance branch at ~line 1346 seeds
from `composed_roles_seed(&mro)` and is explicitly *not* gated on the
constraint being a user-declared role).

Crucially, **`runtime_init.rs` already seeds built-in classes this way**, ~70
lines below the splice (`runtime_init.rs:1896-1929`):

```rust
let ccr = &mut registry.class_composed_roles;
ccr.insert("CompUnit::Repository::FileSystem".into(), vec!["CompUnit::Repository".into()]);
ccr.insert("Int".into(), vec!["Real".into(), "Numeric".into()]);
ccr.insert("Str".into(), vec!["Stringy".into()]);
```

paired with a `RoleDef` for the role name itself in `registry.roles`
(`runtime_init.rs:2054` registers `CompUnit::Repository` as a role with its
three required stub methods). That pairing already produces **all three**
correct answers today, verified on the current build:

```
mutsu -e 'say CompUnit::Repository::FileSystem.^mro.map(*.^name)'
  -> (CompUnit::Repository::FileSystem Any Mu)        # role NOT in the mro  [correct]
mutsu -e 'say CompUnit::Repository::FileSystem.^roles.map(*.^name)'
  -> (CompUnit::Repository)                           #                      [correct]
mutsu -e 'say CompUnit::Repository::FileSystem ~~ CompUnit::Repository'
  -> True   (raku: True)                              #                      [correct]

mutsu -e 'say Str ~~ Stringy; say Str.^roles.map(*.^name); say "x" ~~ Stringy'
  -> True / (Stringy) / True
```

So the "real role-composition path" is not a thing that has to be built; it is
a thing the `X::` block is standing next to and not using.

One further alignment is worth naming, because it makes the data question
trivial: `class_composed_roles` is documented as the **flattened, transitive**
set, and rakudo's `.^roles` default output is likewise transitive
(`X::Syntax::Perl5Var.^roles` → `X::Syntax X::Comp`). The data source and the
destination field have identical semantics, so capture is a copy, not a
derivation.

### 5. Re-measured gap (2026-08-17)

Re-running the ticket's measurement *shape* (not its exact name list): of the
**228** `X::` names used as an expected type in a `throws-like` /
`fails-like` / `isa-ok` across `t/` + `roast/` that are real rakudo Exception
subclasses, **123 cannot be `.new`-ed under mutsu** — every failure with the
same `X::Method::NotFound … new on <class>` signature the ticket reported.

The honest reading is that the headline gap has **not** materially narrowed
since 2026-08-03 despite `register_x` growing 77 → 107: individual named cases
did close (`X::Bind::Slice.new` works now, so the ticket's opening repro is
stale), but the corpus grew alongside. The enumeration of the missing names is
deliberately **not** reproduced here — that belongs to the implementation PR,
mechanically captured (Slice 2), never hand-typed.

## Decision

**Adopt (B): express `X::` role membership through mutsu's existing
composed-role registry path, and take the per-class data verbatim from real
`raku` output.** Concretely:

1. **Register the role-shaped `X::` names as real roles**, not classes. That is
   14 `RoleDef` entries in `registry.roles` (empty bodies — these are pure
   marker/interface roles in rakudo too, and mutsu's exception machinery
   supplies the behaviour), mirroring the existing `CompUnit::Repository` /
   `Distribution` built-in role seeds in the same function. Role-to-role
   composition (`X::Syntax does X::Comp`, `X::IO does X::OS`) goes into
   `role_parents` (or `builtin_role_parents`, whichever the implementation
   finds cleaner) so the transitive walk is a walk, not a copy.

2. **Give the registration helper a third input: a `does` list.** Replace
   `register_x(name, parent)` with a shape that takes
   `(name, parent, does: &[&str])` — or a small `XClassSpec` struct if the
   argument list gets unwieldy. `parent` keeps its current meaning (a real
   superclass, defaulting to `Exception`) and continues to drive `parents` and
   `mro`; `does` is written to `class_composed_roles`,
   `class_direct_composed_roles`, and `class_does_only_roles`, and **never**
   touches `parents` or `mro`.

3. **Retire the `X::Comp::AdHoc` splice** as the first real usage of the new
   input: `X::Comp::AdHoc` becomes `parent = "X::AdHoc"`, `does = ["X::Comp"]`,
   which reproduces rakudo's `.^mro` and `.^roles` exactly instead of the
   current half-right pair.

4. **Correct the 39 existing rows that carry a false superclass**, moving their
   role-shaped ancestor from the parent slot to the `does` slot. This is part of
   the same mechanism change, not a follow-on: leaving them would mean the new
   mechanism coexists with the old mis-modelling it was introduced to remove.

5. **The per-class data comes from real `raku`'s `.^mro` / `.^roles`, captured
   mechanically** — a checked-in script plus a documented recipe, never
   name-prefix inference and never hand transcription. This is orthogonal to
   the mechanism (it is option **C** from the ticket's framing, adopted as the
   *data source* rather than as an alternative mechanism) and is mandatory
   whichever mechanism wins, because §2 shows prefix inference is wrong more
   often than right.

The investigation does not support presenting (A) and (B) as balanced. (B)
costs the same amount of typing, reuses machinery that already has consumers
for `.^roles`, `.^does`, `.^mro`, qualified dispatch and method-candidate
collection, and is demonstrably correct on all three observables today
(§4). (A) is correct on `~~` only, and by construction makes `.^mro` and
`.^roles` *more* wrong the more rows it is applied to.

### Known sharp edge for the implementation

`type_matching.rs`'s **instance** branch seeds the composed-role walk
unconditionally (`composed_roles_seed(&mro)`, ~line 1346), but the
**type-object** (`ValueView::Package`) branch's equivalent walk (~line 1177) is
gated on `resolve_role_key(constraint).is_some()` — i.e. on the constraint
being present in `registry.roles`. Decision item 1 (registering the 14 names as
real roles) is what satisfies that gate, which is also why item 1 is not
optional decoration. Slice 1 must pin **both** shapes explicitly:
`$e ~~ X::Comp` for a thrown instance *and* `X::Comp::FailGoal ~~ X::Comp` for
the bare type object. The `CompUnit::Repository::FileSystem` probe in §4 shows
the paired seed satisfies both today, which is the evidence the gate is
cleared, not an assumption that it is.

## Alternatives considered

1. **(A) Generalise the splice: `register_x(name, parent, does: &[&str])` that
   pushes each `does` entry into `parents` and splices it into `mro`.** This is
   the mechanical generalisation of what `X::Comp::AdHoc` already does by hand,
   and it is the option the source ticket floated first. Rejected: it is right
   about `~~` and wrong about everything else. Applied to the 178 role-doing
   classes it would grow mutsu's `.^mro` disagreement with rakudo from 43 rows
   to ~180, leave `.^roles` empty forever (it writes nothing the `.^roles`
   implementation reads), and put role names into method-resolution order —
   where rakudo puts flattened role *methods*, not the role as an MRO node
   (`class_does_only_roles` exists precisely to encode that distinction). It
   also cements a second, private notion of "does" in a codebase that already
   has a real one, which is the private-dialect risk the repo's Gain/Risk
   section calls a *risk*, not a gain.
2. **(C) as a mechanism — keep `register_x(name, parent)` single-parent and
   compute each class's parent from raku's `.^mro ∪ .^roles`.** Rejected as a
   mechanism (adopted as the data source, per Decision item 5). Collapsing
   `mro ∪ roles` into one parent chain is lossy in both directions: it cannot
   represent `X::Comp::Trait::Unknown` (one real superclass *and* two roles) at
   all, and for the 178 role-doing classes it must either drop the role
   membership (`~~` regresses) or re-introduce the false superclass this ADR
   exists to remove. Its data-capture half, however, is the only sound source
   of per-class truth and is kept.
3. **Register the role-shaped prefixes as classes and inherit from them (status
   quo, extended).** This is what the 36 false-superclass rows do today. It has
   the merit of needing no new mechanism, and is *observationally* fine for a
   `throws-like` that only checks the class name. Rejected: it is exactly the
   thing measured wrong in §1, it makes `.^mro` a permanent lie for the
   majority of exception types, and the prefixes in question are not even
   classes in rakudo (§2's HOW probe) — two of them (`X::Comp`, `X::Syntax`)
   are `ParametricRoleGroupHOW`, and several more are `PackageHOW` namespaces
   that should not be types at all.
4. **Special-case the membership in `type_matches_value`, as
   `X::Await::Died` / `IO::Socket` / `Enumeration` already are
   (`types/type_matching.rs:428-467`).** Rejected: those are three hardcoded
   arms for values that have no `ClassDef` at all; scaling that pattern to 14
   roles × 178 classes would be a hand-written type lattice inside a matching
   function, invisible to `.^roles`, `.^does`, `.^mro`, and method dispatch.
   Existing special cases are debt to pay down, not a pattern to extend
   (CLAUDE.md's "do NOT add new slow-path fallbacks" rule in spirit).
5. **Do nothing / keep the ticket open.** Rejected on the ADR's own framing:
   mutsu *constructs and raises* these types itself, so the gap is not a
   user-facing reflection nicety — it is the interpreter disagreeing with the
   spec about the identity of its own errors. The ticket has been open since
   2026-08-03 and the gap has not narrowed (§5); the reason is that the
   mechanism, not the data, is the blocker.

## Mechanism (phased slices for the follow-up PRs)

Each slice is its own PR off `main`. **None of this lands in the ADR PR.**

### Slice 1 — the mechanism, proven on the one class that already needs it

1. Seed the 14 role-shaped `X::` names as `RoleDef`s in `registry.roles`
   alongside the existing `CompUnit::Repository` / `Distribution` seeds, and
   record their role-to-role composition in `role_parents` /
   `builtin_role_parents`. Verified against `raku` (2026-08-17): all 14 are
   `ParametricRoleGroupHOW`, and there are exactly **two** role-to-role edges —
   `X::Syntax does X::Comp` and `X::IO does X::OS`; the other twelve compose
   nothing.
2. Extend `register_x` to `(name, parent, does)` writing `class_composed_roles`
   + `class_direct_composed_roles` + `class_does_only_roles`, leaving
   `parents`/`mro` untouched by `does`. Every existing call site passes `&[]`
   in this step so the diff is provably behaviour-preserving except where
   step 3 changes it.
3. **Migrate `X::Comp::AdHoc` onto it and delete the splice block**
   (`runtime_init.rs:1813-1829`): `parent = "X::AdHoc"`, `does = ["X::Comp"]`.
4. Pins in a new `t/exception-role-membership.t`, each cross-checked against
   real `raku` **first**:
   - `X::Comp::AdHoc.^mro` is `(X::Comp::AdHoc X::AdHoc Exception Any Mu)` —
     no `X::Comp`;
   - `X::Comp::AdHoc.^roles` contains `X::Comp`;
   - `X::Comp::AdHoc.new ~~ X::AdHoc` and `~~ X::Comp` are both True
     (the behaviour the splice bought, preserved);
   - `X::Comp::AdHoc ~~ X::Comp` on the bare **type object** is True (the
     `resolve_role_key` gate, §"Known sharp edge");
   - `.^does(X::Comp)` agrees with `~~`.
   Existing coverage that must stay green: `t/vcs-conflict-marker.t` (the
   current `X::Comp::AdHoc` consumer) and the `X::AdHoc` pins in
   `t/any-are.t`, `t/backtrace.t`, `t/die-mixin-exception.t`,
   `t/begin-phaser-begintime.t`.

### Slice 2 — mechanical capture of the per-class data

A checked-in script (`scripts/` — sibling in spirit to
`scripts/roast-raku-baseline.sh`) that, given a name list, emits one row per
class of `name`, `.^mro`, `.^roles(:!transitive)`, `.^roles` from a **single**
`raku` process, plus a documented regeneration recipe. Two hard rules,
recorded in the script's header:

- the name list is derived from what mutsu *raises or tests against* (its own
  source, plus `throws-like`/`fails-like`/`isa-ok` expected types in
  `t/`+`roast/`), never from a `X::`-prefix enumeration;
- the output is the input to Slice 3 — nothing in Slice 3 is hand-derived, and
  a name `raku` does not have (test-local classes such as `X::Boom`) is dropped
  by the script rather than guessed at.

The script should also emit the **diff against mutsu's current answers**, so
the 43 wrong-`.^mro` / 52 wrong-`.^roles` rows measured in §1 become a
regenerable, shrinking number rather than a snapshot in this document.

### Slice 3 — land the data

Convert the Slice-2 output into `register_x` calls: correct the ~39 existing
rows whose role-shaped ancestor is currently in the parent slot, and add the
~123 missing classes. Mechanically generated, reviewed as data. If the diff is
uncomfortably large for one PR it may be split by role family (`X::Comp`
cohort, `X::Syntax` cohort, `X::IO`/`X::OS` cohort, the roleless remainder) —
but **not** by "easy first": the ordering must not be chosen to maximise a
test-count delta.

### Slice 4 — probe what it actually unblocks (and record the honest answer)

Only after Slice 3, and framed as measurement rather than payoff:

- `roast/S02-literals/quoting-unicode.t` is the ticket's designated *role-only*
  probe — under the vendored real `Test` module
  (`todo/deep/vendor-real-test-module.md`) it loses six assertions solely
  because `X::Comp::FailGoal ~~ X::Comp` is False, with mutsu's diagnostic text
  already byte-identical to rakudo's. It needs no per-class attribute work, so
  it isolates this ADR's contribution cleanly. (The file's seventh loss, the
  `X::Comp::Group` one, is a separate diagnosis bug —
  `news/2026-08/unterminated-regex-diagnosis.md` — and must not be counted
  here.)
- Re-run the real-`Test` sweep and report the delta **as measured**, including
  if it is small. The ticket already found 7 of 9 regressions in that sweep had
  nothing to do with unregistered classes (two were mutsu raising the *wrong
  class*, which this ADR does not address —
  `news/2026-08/undeclared-variable-is-not-undeclared-symbols.md`). A small
  number is the expected, acceptable outcome; the justification for the work is
  §1, not §4.
- Record any class where correct registration *surfaces* a new failure (e.g. a
  `CATCH` that was silently relying on a false `~~`) as its own finding — that
  is the safety net working, and each one is a real pre-existing bug that the
  wrong hierarchy was masking.

## Acceptance criteria (for the follow-up implementation work, not this PR)

1. `register_x`'s `does` list writes **only** to the composed-role registries;
   a test asserts that no `X::` class's `.^mro` contains any of the 14 role
   names (the anti-regression for the splice pattern).
2. The `X::Comp::AdHoc` splice block is deleted, and `X::Comp::AdHoc`'s
   `.^mro`, `.^roles`, and both instance and type-object `~~ X::AdHoc` /
   `~~ X::Comp` answers match real `raku` exactly.
3. For every class landed in Slice 3, `.^mro` and `.^roles` match the Slice-2
   captured `raku` output. The Slice-2 script's mutsu-vs-raku diff, run over
   the landed set, is **empty** — and it stays in the tree so a future
   divergence is one command away from being visible.
4. `.new` succeeds for every `X::` name in the Slice-2 list (the
   `X::Method::NotFound … new on <class>` signature disappears from the
   corpus).
5. No `t/` or whitelisted-roast regression. A newly-*failing* whitelisted test
   is treated per CLAUDE.md's triage protocol as a real bug — most likely a
   `CATCH`/`when` that was matching through a false ancestor — and fixed, not
   dismissed.
6. Slice 4's measured outcome is recorded honestly in the news entry, including
   a null or near-null result.
7. On completion, `git mv todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`
   to `news/2026-XX/` and rewrite it as an accomplishment, per `todo/README.md`;
   this ADR gains an "Outcome" section and its Status moves to `Accepted`.

## Risks

- **A wrong `does` list is exactly as bad as wrong prefix-guessing.** Writing
  `X::Comp` onto a class that does not do it silently makes a `~~` /
  `CATCH { when X::Comp }` answer True where rakudo says False — a false
  positive that no test necessarily catches, since it *widens* matching. This
  is the single reason Decision item 5 is non-negotiable: the data must be a
  mechanical capture of real `raku` output, regenerable and diffable, never
  typed by hand and never inferred from a name. The Slice-2 diff being part of
  the acceptance criteria (criterion 3) is the mitigation.
- **Correcting the 39 existing false-superclass rows is a behaviour change,
  not a pure addition.** Anything currently relying on `X::Syntax::Confused`
  *inheriting* `X::Comp` (a `.^mro` walk, a `.^parents` check, a method
  resolved through the false parent) changes answer. The role seed keeps `~~`
  and `.^does` True, so the realistic blast radius is introspection and any
  place mutsu itself walks `parents`. Expect CI to find these; that is the
  safety net working, and each hit is a genuine mis-modelling being corrected.
- **Registering 14 new names as roles changes name resolution.** `X::Comp` and
  friends go from `ClassDef` to `RoleDef`, so anything that looks them up as a
  class (including `register_x`'s own MRO-walking loop, which resolves parents
  through `classes`) must be checked. Slice 1 is deliberately small so this
  surfaces before 120 rows depend on it.
- **Attribute coverage is out of scope and may limit the payoff.** `register_x`
  registers no attributes, so a newly-registered class supports `.new` and type
  identity but not necessarily `.new(:payload)` with typed accessors or a
  rakudo-accurate `.message`. The ticket's measurement found every failure had
  the *unregistered* signature rather than an attribute-mismatch signature, so
  this does not block the work — but Slice 4 should expect some tests to move
  from "unknown method new" to a *different* failure rather than to green, and
  should say so rather than counting them.
- **Scope creep into "mutsu raises the wrong class".** Two of the nine
  regressions that motivated the original ticket were mis-*raises*, not
  mis-*registrations*. Fixing them by adding a convenient ancestor would bake
  in inheritance rakudo does not have — the exact failure mode this ADR exists
  to prevent. Read the failure; do not pattern-match the name.

## Implementation status

Recorded here per `docs/adr/README.md` ("record implementation progress inside
the ADR that owns the decision"). Verified on `main` @ `829745e5c`
(2026-08-19) by re-running this ADR's own Slice-2 capture script, so the
numbers below are regenerable rather than transcribed.

| Slice | State |
| --- | --- |
| 1 — mechanism (`register_x` gains `does`; 14 marker roles seeded; `X::Comp::AdHoc` splice deleted) | **Landed** 2026-08-17, #6590 |
| 2 — mechanical raku capture script | **Landed** 2026-08-17, #6591 |
| 3 — land the corrected + missing data | **Landed** 2026-08-18, #6595 |
| 4 — probe what it unblocks, measured honestly | **Deferred** — blocked on `todo/deep/vendor-real-test-module.md` |

Where it lives: `register_x(name, parent, does)` at
`src/runtime/runtime_init.rs:1630` (367 call sites, up from the 107 measured in
§1); the marker roles seeded as `RoleDef`s at `runtime_init.rs:2570-2605`;
role-to-role edges in `registry.role_parents` at `runtime_init.rs:2716-2724`;
the flatten-and-write into `class_composed_roles` /
`class_direct_composed_roles` / `class_does_only_roles` at
`runtime_init.rs:2734-2757`; capture tooling in
`scripts/adr0029-capture-x-exception-data.py` +
`scripts/probe-x-exception-shape.raku`, data in
`TODO_roast/x-exception-role-membership.tsv` (+ `-diff.tsv`); pins in
`t/exception-role-membership.t`.

Measured outcome against the ADR's own §1 baseline:

| | at ADR time | 2026-08-19 |
| --- | --- | --- |
| wrong `.^mro` (of which false superclass) | 43 (36) | **0** |
| wrong `.^roles` | 52 (all empty) | 15 (13 cosmetic, 2 real — see below) |
| classes matching raku byte-for-byte | 47 of 105 | **357 of 373** |
| `.new` fails (`X::Method::NotFound`) | 123 | **1** (`X::TooLateForREPR`) |

Acceptance criteria 1, 2 and 5 are met; criterion 4 is met for 372 of 373
names. Criteria 3 and 6 are not yet met, and the reasons are tracked as a
five-item residue list — **not** as a re-opened design question — in
[`todo/deep/exception-class-hierarchy-is-mostly-unregistered.md`](../../todo/deep/exception-class-hierarchy-is-mostly-unregistered.md),
which is the ticket to pick up next. In brief:

- **R1** — Slice 3 grew the marker-role list from 14 to 16 without re-running
  the role-to-role edge measurement, so `X::Role::Attribute does X::RoleApplier`
  is unseeded and two classes answer `~~ X::RoleApplier` False where rakudo
  says True. The prose comment at `runtime_init.rs:2716-2718` asserting "exactly
  two edges exist" is a hand-maintained claim about data, which Decision item 5
  forbids everywhere else; it should be derived.
- **R2** — `X::TooLateForREPR` is the one unregistered class. Its rakudo shape
  is a role-as-superclass pun (`X::Comp` is both an MRO entry *and* a composed
  role), which contradicts **acceptance criterion 1** as globally worded. The
  criterion needs amending to "no role name in any MRO except where the captured
  raku data says otherwise", driven off the Slice-2 TSV rather than a hardcoded
  rule.
- **R3** — criterion 3 ("the diff is empty") is unreachable as written: 13 rows
  differ only because rakudo's `.^roles` emits a role twice when reached both
  directly and through a superclass, while mutsu dedups. The composed-role
  *set* matches in all 13. The script's verdict should compare sets (keeping
  raw raku output in the data TSV); mutsu must **not** learn to replicate the
  duplicate emission.
- **R4** — the 16 marker names are still dual-registered as both `ClassDef`
  (`runtime_init.rs:1676-1679`) and `RoleDef`. Load-bearing today (it is what
  makes R2 expressible), but undocumented, so the next reader will read it as
  pre-ADR-0029 residue and delete it.
- **R5** — Slice 4's designated *role-only* probe has expired:
  `roast/S02-literals/quoting-unicode.t` is now whitelisted and passes 101/101,
  and `X::Comp::FailGoal ~~ X::Comp` is True for both instance and type object.
  Slice 4's remaining content is exclusively the vendored-real-`Test` sweep.

Status moves to `Accepted` — with an `Outcome` section replacing this one —
once R1-R4 land and Slice 4 is run or formally re-scoped.
