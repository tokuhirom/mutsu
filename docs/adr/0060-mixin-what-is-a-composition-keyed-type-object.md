# ADR-0060: A role-mixed value's `.WHAT` is a composition-keyed type object, not the shared base or a per-instance fork

- Status: Accepted (implemented)
- Date: 2026-08-22
- Related: ADR-0013 (container interior mutability / `GcCell`), ADR-0001 §7 (GC strategy —
  Bacon-Rajan cycle collector), ADR-0047 (type identity is a declaration site, not a registry name)
- Addresses: `todo/deep/mixin-what-identity-not-per-composition.md`

## Context

`dispatch_what()` (`src/runtime/methods_introspect.rs`) handles `ValueView::Mixin(inner, _)` — a
value produced by `does`/`but` on a native representation (`%h does SomeRole`), or by punning a
role via `.new()` — by recursing into the inner base value's own `.WHAT` and discarding the
mixin's role/type markers entirely:

```rust
ValueView::Mixin(inner, mixins) => {
    if let Some(allo) = crate::value::types::allomorph_type_name(inner, mixins) {
        return Ok(Value::package(Symbol::intern(&allo)));
    }
    return self.call_method_with_values(inner.as_ref().clone(), "WHAT", args.clone());
}
```

So `(%h does R).WHAT` returned the plain shared `Package("Hash")` — the exact same value every
other `Hash` in the process shares — even though `%h.^name` correctly reports `Hash+{R}` (a
different code path, `dispatch_caret_name`, which does look at the role markers). Confirmed
2026-08-22 against current `main` (unchanged since the ticket was filed):

```raku
my role R { }
my %h; %h does R;
say %h.^name;         # Hash+{R}   (mutsu: correct)
say %h.WHAT.^name;     # mutsu: Hash   -- raku: Hash+{R}
say %h.WHAT === Hash;  # mutsu: True   -- raku: False
```

### A previously-tried fix, re-verified broken

The ticket recorded a same-session attempt that made `.WHAT` on a `Mixin` reuse the *instance's
own* `overrides` `Gc` handle: `Value::mixin_parts(Arc::new(base_what), mixins.clone())`. Re-applied
and re-measured on current `main` as part of this ADR's verification: it does fix the repro above,
but breaks `roast/S14-roles/instantiation.t` test 4 exactly as documented —

```
--- mutsu repro (with the naive fix) ---
Hash+{R}
Hash+{R}
False
--- S14 instantiation.t (with the naive fix) ---
# Failed test 'Punned role classes have the same .WHAT'
#   at roast/S14-roles/instantiation.t line 24
```

Root cause of the break: `SampleRole.new()`'s two separately-constructed instances each get their
OWN `Gc<MixinOverrides>` (per-instance node, `Value::mixin` always allocates fresh — see
"Mechanism" below). That map carries not just the role-composition marker
(`__mutsu_role__SampleRole`) but a monotonic **per-application order stamp**
(`__mutsu_role_seq__SampleRole`, `next_instance_id()`-derived — used to resolve method-name
collisions between mixed-in roles by later-wins precedence, `todo/tickets/mixin-role-order-not-tracked.md`).
`===` on two `Mixin` values (`src/runtime/utils/shaped.rs:238-240`) does a **deep structural
compare of the whole overrides map**: `a_inner.eqv(b_inner) && a_mix == b_mix`. Two structurally
different `__mutsu_role_seq__` stamps make `a_mix == b_mix` false even when the two instances are,
semantically, the exact same composition. Reusing "whichever `overrides` map the instance already
has" is therefore never correct: it conflates composition identity with per-instance bookkeeping
that happens to live in the same flat map.

## Decision

Give a `Mixin` value's `.WHAT` a **composition-keyed anonymous type object**, cached process-wide,
keyed by `(base type name, sorted set of composition-defining role markers)` — excluding every
per-instance/bookkeeping key in the same flat `MixinOverrides` map. Two `Mixin` values with the
same base type and the same role set get the identical (by content, and by construction the same
underlying `Gc<MixinOverrides>` node) `.WHAT`; two values with a different composition never share
one; `.^set_name` on the shared type object is visible to every current AND future instance of that
exact composition, and to no other composition — matching Rakudo exactly (see "Verification" for
the case table this was checked against).

### Why this is the right level of caching (verified against Rakudo first)

Before designing the cache shape, the operative Rakudo semantics were nailed down directly, since
the ticket's own two data points (`Hash::Restricted`, `S14 instantiation.t`) constrain the *ends*
but not the general rule:

```raku
my role R { } my role S { }
my %h1; %h1 does R;  my %h2; %h2 does R;  my %h3; %h3 does S;  my @a; @a does R;
say %h1.WHAT === %h2.WHAT;   # True  -- same base + same role set
say %h1.WHAT === %h3.WHAT;   # False -- different role set
say %h1.WHAT === @a.WHAT;    # False -- different base type
my $x1 = 1 but R; my $x2 = 2 but R;
say $x1.WHAT === $x2.WHAT;   # True  -- `but` on a scalar is composition-keyed too

%h1.WHAT.^set_name("Hash(restricted)");
my %h4; %h4 does R;          # constructed AFTER the rename
say %h2.^name;                # Hash(restricted) -- an unrelated PRE-existing instance sees it
say %h4.^name;                # Hash(restricted) -- so does a FRESH instance created after
say %h3.^name;                # Hash+{S}         -- a DIFFERENT composition is untouched

role RP[::T] { method get { T } }
my $a = 1 but RP[Int]; my $b = 2 but RP[Int]; my $c = 3 but RP[Str];
say $a.WHAT === $b.WHAT;      # True  -- same type argument
say $a.WHAT === $c.WHAT;      # False -- different type argument is a different composition
```

All six data points are consistent with a single rule: **Rakudo builds and permanently caches one
anonymous type object per (base type, role set, role type-arguments) triple** — never per instance,
never shared with the base type, and the cache entry outlives any particular instance (the
`%h4`-after-rename case rules out any design where the shared identity is only reachable via a
still-live instance, e.g. a weak/GC-swept table — see "Rejected: GC-participating cache" below).

This is not a new discovery for this codebase: `news/2026-08/role-composition-memo-key-raku-case-table.md`
already established the identical Rakudo rule from the deferred-role-body-memoization side ("Rakudo
builds (and caches) an anonymous type per (base type, role) pair... an `Int+{R}` is a different type
from a `Str+{R}`") and `Registry::composed_role_bodies` already keys its `mixin:{base_type}:{role_name}`
memo exactly this way (`src/runtime/types/roles.rs:472`). This ADR's cache is the identity
counterpart of that same underlying Rakudo mechanism, not an unrelated new one.

### The composition key

`MixinOverrides` (`type MixinOverrides = HashMap<String, Value>`, `src/value/mod.rs:75`) is a flat
map with no structural separation between composition-defining data and per-instance data — five
distinct construction call sites (native `but`, role `does`/`but`, punned-role `.new()`, allomorphs,
mutation write-back) all funnel into the same shape, distinguished only by a `__mutsu_*__`
key-prefix convention. The composition key extracts exactly the subset that already has a defined
meaning as "the composition":

- **Include**: for every `__mutsu_role__{name}` marker present, the tuple `(role_name, role_id,
  typeargs)`:
  - `role_id` — `__mutsu_role_id__{name}`, already recorded by `roles.rs` specifically "so that
    different lexical roles with the same name (e.g. two `my role A {}` in different scopes)
    produce distinct mixin maps" (`src/runtime/types/roles.rs:523-536`). Reusing it here is the
    same "declaration site, not name" principle ADR-0047 already applies to type identity
    elsewhere in this codebase.
  - `typeargs` — `__mutsu_role_typeargs__{name}`, stringified via the existing `what_type_name`
    per element (verified above: `RP[Int]` and `RP[Str]` are different compositions).
  - Sorted (the map has no inherent order) and joined into a stable string.
- **Exclude**: `__mutsu_attr__*` (per-instance role-attribute values — two `role R { has $.x };
  5 does R(3)` and `7 does R(9)` must share one `.WHAT` despite different `$.x`), `__mutsu_role_seq__*`
  (the per-application order stamp that broke the naive fix above), `__mutsu_role_param__*`
  (derived from data already captured by typeargs), `__mutsu_type_name__` (the `.^set_name` target
  itself — mutable state ON the cache entry, not part of its key), and every non-role-composition
  key already known to live in this map for unrelated purposes (`__mutsu_var_target`,
  `__mutsu_how_target`, `__mutsu_topic_ro__`, `"Str"` for allomorphs, `__mutsu_language_revision`).
- **Base type name**: the already-computed `base_what` (`.WHAT` of `inner`, recursively) stringified
  — reusing `dispatch_what`'s existing recursive call rather than a second, possibly-diverging
  type-name helper, so typed-array/hash parameterization (`Hash[Int,Str]`) is picked up for free.

This key derivation is not new machinery: `role_mixin_suffix_excluding()` (`src/value/types.rs:110-130`)
already walks the same map filtering `__mutsu_role__` keys to build the `Base+{Role,...}` *display*
name that `.^name` uses — proof this filtering is already load-bearing and tested, just not
currently reused for identity.

### The cache

A plain `HashMap<String, Value>` — `Registry::mixin_what_cache` — mapping the composition key
string to the shared `.WHAT` `Value` (a `Mixin(Arc<base_what>, Gc<MixinOverrides>)` whose overrides
node starts empty and is populated lazily, in place, by `.^set_name`). `dispatch_what()`'s `Mixin`
arm becomes: compute `base_what` (unchanged), derive the key, look up-or-insert the cache entry,
return it.

`.^set_name` called *directly on an instance* (`$obj.^set_name(...)`, not via `.WHAT`) is redirected
the same way: instead of writing into the instance's own per-instance `overrides` (today's
behavior, which only round-trips through *that one instance's* aliases — insufficient per the
`%h1`/`%h2` case above), it derives the same composition key from the instance's `overrides` and
writes into the cache entry's shared node via the existing `gc_contents_mut` primitive
(`docs/adr/0013`) — the exact same in-place-aliased-write mechanism `.^set_name` already uses, just
retargeted to the composition-keyed node instead of the instance-keyed one. `.^name`'s fast path
(`dispatch_caret_name`) and `dispatch_classhow_method`'s `"name"` handler read `__mutsu_type_name__`
from the same cache entry (falling back to the synthesized `Base+{Role,...}` name when no entry, or
no rename, exists) instead of the instance's own `overrides`.

### Rejected: a `Gc`-participating / weak-referenced cache

The ticket flagged leak risk ("a hot loop doing `$x but SomeRole` ... with varying attribute values
but the same role set") and asked whether the cache needs to be `Gc`-managed so the collector can
reclaim an entry once its last instance drops — mirroring the existing `WeakGc<T>` idiom already
used for `WeakSub` and the not-yet-consumed-`LazyList` registry.

This is the wrong mechanism here, for two independent reasons:

1. **It cannot reproduce the verified semantics.** The `%h4`-after-rename case above proves the
   renamed type object must be reachable independent of any live instance — a weak-table entry
   would already be collected (nothing else strongly references it between `%h1` going out of
   scope and `%h4` being constructed) and the rename would silently vanish. Rakudo's own type
   objects are, in practice, never collected either; a plain permanent table is not a compromise
   here, it is the accurate model.
2. **The leak worry does not apply once the key excludes instance data.** A "hot loop doing `$x but
   SomeRole` with varying attribute values but the same role set" hits the exact same key on every
   iteration (attribute values are excluded from the key by construction), so it grows the cache by
   **one** entry total, not once per iteration. What actually bounds the cache's size is the number
   of distinct (base type, declared role set, type-argument) combinations the running program ever
   composes — which, for ordinary programs, is bounded by source call sites, not by runtime
   iteration count. This is exactly the same growth profile the codebase already accepts,
   unremarked, for two structurally identical existing tables: `Registry::classes` (punned-role
   identity, `ensure_role_punned_to_class`, keyed by role name — `src/runtime/registration_class_augment.rs:1079`)
   and `Registry::composed_role_bodies` (deferred-body memoization, keyed by `"mixin:{base}:{role}"`).
   Neither is `Gc`-participating; both are permanent `HashMap`/`HashSet` fields on `Registry`. This
   cache is the same shape for the same underlying reason (Rakudo's own type-identity table is
   permanent), not a new risk category.

An adversarial program that composes unboundedly many *distinct declared* roles at runtime (e.g.
`EVAL "role R{$i} {}"` in a loop) would grow this cache unboundedly — but that is already true of
`Registry::classes`/`composed_role_bodies` today, is not something this ADR's scope introduces, and
is not a realistic shape for the actual blockers this fixes (`Hash::Restricted`'s `is restricted`
trait names a single fixed role).

## Blast-radius audit of other `Mixin` consumers

`ValueView::Mixin` is matched at ~195 sites across `src/` (`grep -rn "ValueView::Mixin" src/`).
Categorized by what each class of site assumes, to determine which are at risk from changing what
`.WHAT` returns:

- **The overwhelming majority (~110+ sites)** — arithmetic/coercion/comparison/numeric-limit
  helpers (`src/runtime/utils/compare.rs`, `utils/rat.rs`, `vm/vm_comparison_ops.rs`,
  `vm/vm_value_helpers.rs`, `runtime/ops_reduction.rs`, etc.) — unwrap to `inner` and never look at
  `.WHAT` or type identity. **Unaffected**: this ADR does not change `ValueView::Mixin`'s
  representation, construction, or how `inner` is read, only what a NEW call to `.WHAT` computes.
- **Composition-set consumers (~15-20 sites)** — `what_type_name`/`role_mixin_suffix_excluding`
  (`src/value/types.rs`), `.isa`/`.does`/type-check-against-a-role-set (`src/value/types_isa.rs`,
  `src/runtime/types/type_matching.rs`), method-dispatch chain walking (`src/runtime/receiver_class.rs`'s
  `mixin_chain`). These all read the role markers directly off a value's OWN `overrides`, not via a
  call to `.WHAT` — confirmed by grepping `"WHAT"` across every file in this category: none of them
  call it. **Unaffected**: they already treat the instance's own `overrides` as ground truth and do
  not route through `dispatch_what`.
- **Equality/identity (`src/value/types_eqv.rs`, `src/runtime/utils/shaped.rs`)** — the sites that
  actually implement `===`/`eqv` for two `Mixin` values (quoted and analyzed above). These are the
  ones this ADR's correctness depends on, not ones it risks: `values_identical`'s `(Mixin, Mixin) =>
  a_inner.eqv(b_inner) && a_mix == b_mix` already does the right thing once `.WHAT` returns a proper
  shared-node `Mixin` (no changes needed there), and its `(Mixin, _) | (_, Mixin) => false` arm is
  what makes `%h.WHAT === Hash` correctly flip from mutsu's current (wrong) `True` to raku's `False`
  as a **side effect** of this fix, not a separate change.
- **`dispatch_what` itself, `dispatch_classhow_method`'s `"set_name"`/`"name"` handlers, and
  `dispatch_caret_name`** (all in `src/runtime/methods_introspect.rs` /
  `src/runtime/methods_classhow_dispatch.rs`) — the actual edit sites, covered above.
- **Method-name-lookup sites that list `"WHAT"` as a pseudo-method name** (`src/runtime/methods_call_dispatch.rs`,
  `src/runtime/methods_dispatch_match.rs`, `src/runtime/class_introspection.rs`) — pure dispatch
  routing (`"WHAT" => dispatch_what(...)`), not consumers of the *result*. **Unaffected**.

No site inspects the result of a `.WHAT` call on a `Mixin` value and assumes it unwraps straight to
a `Package` — the only place that assumption lived was `dispatch_what` itself.

## Verification

- Repro from the ticket now matches `raku` exactly:
  ```
  say %h.^name;         # Hash+{R}
  say %h.WHAT.^name;     # Hash+{R}  (was: Hash)
  say %h.WHAT === Hash;  # False     (was: True)
  ```
- The six-point composition-identity case table above (same base+role → True, different role/base/
  typearg → False, rename propagates to a pre-existing sibling instance AND a freshly-constructed
  one, a different composition is untouched by an unrelated rename) — verified against `raku`
  first, then reproduced identically by the implementation.
- `roast/S14-roles/instantiation.t` (`MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/S14-roles/instantiation.t`) —
  all 19 subtests pass, including test 4 ("Punned role classes have the same .WHAT") and test 5
  (`.WHAT.gist` reporting `(SampleRole)`, not `(SampleRole+{SampleRole})` — the role-punning
  exclusion in `role_mixin_suffix_excluding` already handles this and is now exercised via `.WHAT`
  for the first time).
- `t/metamodel-set-name.t` (existing local pin for `.^set_name` on a `but`-mixed instance,
  including the "an alias sharing the same object observes the rename" case) — unaffected by the
  redirect from instance-keyed to composition-keyed storage, since a single instance and its alias
  are trivially "the same composition."
- `make test`: all 868 `cargo test` unit/integration tests, plus the full `t/` TAP suite
  (3353 files, 31292 assertions) — clean.
- `roast/S32-exceptions/misc.t` (the only whitelisted roast file using anonymous `but role :: {}`
  syntax) — still passes; it only checks that a bare method-less anon role composition throws
  `X::Method::NotFound`, not `.WHAT` identity across separate evaluations.

### A third `.^name` fast path had to be found and retargeted

Wiring `dispatch_caret_name` and `dispatch_classhow_method`'s `"name"`/`"set_name"` handlers through
the cache was not sufficient on its own: `%h1.^set_name(...)` then `%h2.^name` (a *different*
pre-existing instance of the same composition) kept reporting the un-renamed synthesized name. The
actual `.^name` call for a `Mixin` target does not reach either of those interpreter-level handlers
first — `call_method_with_values()`'s documented two-tier dispatch (fast path first, see
`CLAUDE.md`) tries `native_method_0arg()` (`src/builtins/methods_0arg/mod.rs`) before either. That
fast path had its own, third, independent `"^name"` special case for `Mixin` values (reading
`mixins.get("__mutsu_type_name__")` off the INSTANCE's own `overrides` directly, with no fallback to
any shared node) and returned early, so the interpreter-aware handlers were never reached at all for
a direct `.^name` call. `native_method_0arg` is a pure function with no `&Interpreter` access by
design (`CLAUDE.md`'s "Fast path" tier), so it cannot itself consult the registry-backed cache — the
fix was to delete that fast-path special case entirely (it fell through, via its final
`native_method_0arg(inner, method_sym)` delegation, to `None`, reaching the slow path), not to try
to extend it. This is recorded because it is the kind of gap `todo/deep/mixin-what-identity-not-per-composition.md`'s
own "wide blast radius across dispatch code" warning anticipated, and it was found only by tracing
actual execution (a temporary `eprintln!` behind an env var in the cache lookup, per `CLAUDE.md`'s
debugging guidance, showed the cache was never even queried for this call shape) rather than by
static code reading alone.

### Known follow-up gap, out of this ADR's scope

Composing the SAME anonymous role literal twice (`sub mk() { Foo.new but role :: { ... } }` called
twice) should yield `$o1.WHAT === $o2.WHAT` `True` per `raku` — but mutsu now reports `False`,
because mutsu's anonymous-role identity marker is assigned per runtime evaluation of the literal,
not per declaration site (ADR-0047's principle, not yet applied to anon roles specifically). Before
this ADR this was masked by `dispatch_what`'s old full-unwrap behavior (any two mixins on the same
base type were accidentally `===`, regardless of role identity). Verified this is not exercised by
any currently whitelisted roast test (`roast/S32-exceptions/misc.t`, the only whitelisted file using
`but role ::`, never checks cross-evaluation `===`) or local `t/` test. Filed separately as
`todo/tickets/anon-role-mixin-identity-not-declaration-site-stable.md` — a narrower, pre-existing
gap in anonymous-role identity minting itself, not in this ADR's cache mechanism.
