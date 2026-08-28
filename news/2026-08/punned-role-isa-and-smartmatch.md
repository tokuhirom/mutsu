# A punned role's instance did not isa/smartmatch its pun

`R.^pun` — the concrete class a role generates for its instances — is
represented internally as `Mixin(Package(role_name), overrides)` (see
`punned_role_type_object` in `methods_mixin_what_cache.rs`, ADR-0060). Three
independent places that extract a "type name" from an isa/smartmatch argument
enumerated `Package`/`Str`/`Instance` but had no case for a `Mixin`, so a pun
used as the RHS silently fell through to a generic display-string comparison
that could never match:

```raku
role R1 { }
my $o = R1.new;
say $o.isa(R1.^pun);   # was False, raku: True
say $o ~~ R1.^pun;     # was False, raku: True
```

Note that `$o.isa(R1)` (the bare role, not its pun) is correctly `False` in
both implementations — roles are excluded from nominal `.isa` checks — and
that exclusion had to be *preserved* for the bare-role argument while being
*skipped* for the pun, since both stringify to the same name "R1" but only
the bare role is excluded in raku.

Three fixes, all following the same pattern (unwrap the `Mixin` to its inner
`Package`/`Instance`, then proceed with the existing name-based logic):

- `Value::isa` (`src/runtime/methods_mixin_dispatch.rs`): added a `Mixin` arm
  that unwraps to the inner name and skips the "roles are excluded" rule
  (that rule now only fires when the argument is a bare `Package`, i.e. the
  literal role, not its pun).
- Smartmatch (`src/runtime/seq_helpers/smart_match.rs`): added a
  `(_, ValueView::Mixin(pun_inner, pun_mixins))` arm (gated on a
  `__mutsu_role__*` marker, i.e. only for a role pun) that recurses into
  `smart_match_inner(left, pun_inner)`.
- `nqp::istype` (`src/runtime/nqp_ops.rs`): its type-name extraction gained
  the same `Mixin` unwrap. This mattered beyond the pun case itself: the
  vendored real `Test.rakumod`'s `isa-ok` calls `nqp::istype($var,
  $type.WHAT)` whenever its expected type isn't a `Str`, and a pun's own
  `.WHAT` is *also* a `Mixin` (via the same ADR-0060 composition cache), so
  `isa-ok $obj, R1.^pun, "..."` was False under `MUTSU_REAL_TEST=1` even
  after the plain `.isa`/`~~` fixes above. This closed
  `roast/S12-coercion/coercion-methods.t`'s "Roles" subtest under both the
  native and the real `Test` provider.

Regression test: `t/role-pun-isa-smartmatch.t` (green under `raku` too).
