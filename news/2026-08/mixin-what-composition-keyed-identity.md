# A role-mixed value's `.WHAT` is now a composition-keyed type object (ADR-0060)

`(%h does R).WHAT` used to return the plain shared `Hash` `Package` — the exact same value every
other `Hash` in the process shares — even though `%h.^name` correctly reported the composed name
`Hash+{R}` via a different code path. This broke `Hash::Restricted`'s `is restricted` trait, which
calls `v.var.WHAT.^set_name("$name(restricted)")` right after `does`-mixing a restriction role onto
a variable, intending to rename only that variable's type — mutsu instead renamed every `Hash` in
the program (or, once `.^set_name` on a builtin `Package` was made a no-op to stop that, renamed
nothing at all).

The fix, designed and recorded as
[ADR-0060](../../docs/adr/0060-mixin-what-is-a-composition-keyed-type-object.md): give a role-mixed
value's `.WHAT` a genuine, permanently-cached anonymous type object per **composition** — `(base
type, role set, role type-arguments)` — shared by every value with that exact composition, never
shared with the base type, and never forked per instance. Verified against `raku` first:

```raku
my role R { } my role S { }
my %h1; %h1 does R;  my %h2; %h2 does R;  my %h3; %h3 does S;
say %h1.WHAT === %h2.WHAT;   # True  -- same base + same role set
say %h1.WHAT === %h3.WHAT;   # False -- different role set
%h1.WHAT.^set_name("Hash(restricted)");
my %h4; %h4 does R;          # constructed AFTER the rename
say %h2.^name;                # Hash(restricted) -- an unrelated pre-existing instance sees it
say %h4.^name;                # Hash(restricted) -- so does a fresh instance created after
say %h3.^name;                # Hash+{S}         -- a different composition is untouched
```

mutsu now matches every one of these exactly, and a previously-tried "reuse the instance's own
`overrides` map" fix (which broke `roast/S14-roles/instantiation.t`'s "punned role classes have the
same `.WHAT`" invariant) is superseded by a proper composition key: base type name plus the sorted
`(role_name, role_id, typeargs)` triples a mixin's `overrides` records, deliberately excluding
per-instance data (attribute values, the per-application-order stamp that broke the earlier
attempt) — mirroring `Registry::composed_role_bodies`'s existing `"mixin:{base}:{role}"` memo key,
which already encodes the identical underlying Rakudo mechanism (Rakudo builds and permanently
caches one anonymous type per (base type, role) pair) for a different purpose.

Three independent dispatch paths had to be retargeted at the same cache: `dispatch_what()`,
`dispatch_classhow_method`'s `"set_name"`/`"name"` handlers, and `dispatch_caret_name` (the `.^name`
fast path) — plus a fourth, previously-unnoticed fast path in `native_method_0arg` that had its own
`"^name"` special case reading an instance's own `overrides` directly, bypassing every
interpreter-aware handler. That pure-function fast path cannot see the registry-backed cache by
construction, so its `"^name"` special case for `Mixin` values was removed rather than extended,
falling through to the slow path instead.

Verified against `roast/S14-roles/instantiation.t` (all 19 subtests, including the punned-role
identity invariant), `t/metamodel-set-name.t`, and the full `make test` suite (868 `cargo test`
cases plus all 3353 `t/` files) with no regressions. One narrower, pre-existing gap was found and
deferred separately: composing the same *anonymous* role literal twice (`but role :: { ... }`
evaluated twice) should share one `.WHAT` per `raku`, but mutsu's anonymous-role identity is minted
per evaluation rather than per declaration site — not exercised by any whitelisted test today, filed
as `todo/tickets/anon-role-mixin-identity-not-declaration-site-stable.md`.
