# A punned role's attributes live in the instance, so `$!attr` works

Instantiating a role directly puns it to a class. Reading one of its attributes
privately from inside a role method threw:

```raku
role R {
    has $.parent;
    method show { say $!parent }
}
R.new(:parent(42)).show;
```

```
P6opaque: no such attribute '$!parent' on type R in a R when trying to get a value
```

raku prints `42`. The public accessor (`$r.parent`) worked, which made the
failure look narrower than it was.

## Root cause

`.new` on a bare role built a `Mixin` whose *inner instance had an empty
attribute cell*, and stashed the attribute values as `__mutsu_attr__<name>`
markers in the mixin map instead. The public accessor is served out of those
markers by the mixin dispatcher, so it worked; but a private access (`$!attr`)
goes through the instance's own attribute cell, found nothing, and — with the
punned name declaring no attributes either — raised the Rakudo P6opaque error.

The markers were also a second store: a role method that wrote `$!attr` wrote to
the instance cell, while the accessor kept reading the marker, so the two would
have diverged even if the read had resolved.

## The fix

The instance cell becomes the store of record.

- Role punning now seeds the punned instance's own attribute map with the
  attribute values. The `__mutsu_attr__` markers are still written — the
  accessor path and the several "is this a role mixin?" checks key off them —
  but they are construction-time seeds, not the live value.
- The mixin accessor prefers the wrapped instance's cell whenever it carries the
  attribute, so it cannot serve a stale seed after a private write.
- Building the attribute set visible to a role method body no longer overlays a
  marker on top of an attribute the instance already has, for the same reason.

## Impact

This was blocking two `DBIish` test files (`docs/batteries/database.md`), which
instantiate the `DBDish::ErrorHandling` role directly — `DBDish::ErrorHandling.new(:parent(Nil))` —
and whose methods read `$!parent` and `$!last-exception`. `05-mock` went from
running 0 of its planned 16 tests to running 12 of them (11 passing) before
aborting on an unrelated error, and `48-sqlite-errors` no longer fails on the
attribute at all — it now reaches the `NativeLibs` blocker like its siblings.

## Scope

Only *scalar* role attributes are seeded into the cell. A `@`/`%` role attribute
is already served end-to-end by the marker path — `has %!h handles <AT-KEY
ASSIGN-KEY>` routes element access through the delegation forwarder, which reads
and writes the marker — and seeding the cell as well would give that path two
stores to disagree about. Making the container case cell-authoritative too means
reworking the delegation forwarder's attribute lookup; that is left as a `TODO`
at the seeding site.

Pinned by `t/role-pun-private-attribute.t`.
