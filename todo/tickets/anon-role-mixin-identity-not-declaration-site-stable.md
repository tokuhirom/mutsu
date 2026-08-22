# A `but`/`does` mixin of the *same anonymous role literal*, evaluated twice, should share one `.WHAT` — mutsu gives two

## Root cause

ADR-0060 (`docs/adr/0060-mixin-what-is-a-composition-keyed-type-object.md`) gave a role-mixed
value's `.WHAT` a genuine per-composition identity, keyed by `(base type, role name, role_id,
typeargs)` via `crate::value::types::mixin_composition_key`. `role_id` (recorded in a mixin's
`overrides` under `__mutsu_role_id__{name}`, from `self.registry().roles.get(role_name).role_id`)
exists specifically "so that different lexical roles with the same name (e.g. two `my role A {}` in
different scopes) produce distinct mixin maps" (`src/runtime/types/roles.rs:523-536`).

For a *named* role (`my role R {}`), `role_id` is stable across every runtime evaluation of `R` —
it is assigned once, at declaration/registration time. But for an *anonymous* role literal (`but
role :: { ... }`), verified against `raku` that the SAME textual literal, evaluated twice (e.g.
inside a function called twice), must still compose to the identical `.WHAT`:

```raku
class Foo { has $.x = 1; }
sub mk() { return Foo.new but role :: { has $.tag = "hello" }; }
my $o1 = mk();
my $o2 = mk();
say $o1.WHAT === $o2.WHAT;   # raku: True
```

mutsu (after ADR-0060) prints `False`. This means mutsu's anonymous-role identity marker
(`__ANON_ROLE_{id}__`, or whatever backs `role_id` for an anonymous role — the exact mechanism
needs re-confirming; `role_mixin_suffix_excluding` in `src/value/types.rs` already deliberately
masks anon role names from display, "leave anon mixins un-suffixed... rather than leaking the
internal name", but that is a DISPLAY concern, separate from this IDENTITY concern) is assigned
**per runtime evaluation** of the `but role :: {...}` expression, not **per declaration site** the
way ADR-0047 ("type identity is a declaration site, not a registry name") and this anon-role
literal's own Rakudo semantics require.

## Why this was masked until now

Before ADR-0060, `dispatch_what()`'s `ValueView::Mixin` arm fully unwrapped to the base value's own
`.WHAT` (`inner.WHAT`), discarding ALL role composition data — so `$o1.WHAT === $o2.WHAT` was
accidentally `True` for ANY two mixin values sharing the same base type, regardless of role
identity. ADR-0060 correctly stopped discarding role identity (needed for `Hash::Restricted` and
the general `.WHAT` composition-identity fix), which is what exposes this separate, narrower,
pre-existing gap: anonymous role identity itself was never verified against Rakudo's declaration-
site semantics.

## Why this is deferred rather than fixed inline in ADR-0060

- It is a genuinely separate mechanism (how an anonymous role's own identity/`role_id`-equivalent
  is minted) from ADR-0060's actual scope (how a `.WHAT` *cache* is keyed off whatever identity
  already exists on a mixin's `overrides`). Fixing it requires finding and changing where anonymous
  role registration assigns its id/name (likely in the parser or `runtime/registration_*` role
  handling, not touched by ADR-0060), which is out of that ADR's blast radius.
- Verified NOT exercised by any currently whitelisted roast test or `t/` local test: the only
  whitelisted roast file using `but role ::` syntax is `roast/S32-exceptions/misc.t`, which only
  checks that a bare method-less anon role composition throws `X::Method::NotFound` — it never
  checks `===` identity across two separate evaluations. `t/metamodel-set-name.t` only evaluates
  its anon role literal once. So this is a real Rakudo-compatibility gap, not (yet) a regression
  against anything currently passing.

## Repro

```raku
class Foo { has $.x = 1; }
sub mk() { return Foo.new but role :: { has $.tag = "hello" }; }
my $o1 = mk();
my $o2 = mk();
say $o1.WHAT === $o2.WHAT;   # raku: True, mutsu: False
```

## Affected files (where to start)

- Wherever an anonymous role literal (`role :: { ... }` / `but role { ... }`) is registered and
  assigned whatever backs `__mutsu_role_id__`/`__ANON_ROLE_{id}__` — likely
  `src/runtime/types/roles.rs` and/or the parser's anon-role handling. Needs tracing (AST dump +
  `rust-gdb` breakpoint per `CLAUDE.md`'s debugging guidance) to confirm whether the id is derived
  from a per-declaration-site counter (assigned once, e.g. at parse/compile time) or a per-
  evaluation counter (assigned fresh every time the enclosing expression runs) — the fix is to make
  it the former, mirroring how `my role A {}`'s `role_id` already works.
- `src/value/types.rs`'s `role_mixin_suffix_excluding` (the anon-role display-masking logic) is
  NOT itself broken and should not need changing — this ticket is about identity, not display.
