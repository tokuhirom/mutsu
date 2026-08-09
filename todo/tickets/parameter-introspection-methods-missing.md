# Parameter introspection gaps: `usage-name`, `constraint_list`, `default` (when absent)

## Affected tests

- `t/http-router-named-urls.t` (Cro::HTTP dist) — after the `-> +a` parse fix
  (ticket `tap-pointy-param-sigilless-plus-slurpy.md`), every `route {}` block in
  the file dies during `definition-complete` → `!generate-urls` → `TWEAK` →
  `Cro::HTTP::Router::LinkGenerator::signature-to-sub`:
  1. `No such method 'constraint_list' for invocant of type 'Parameter'`
     (LinkGenerator.rakumod:25, `extract-static-part`)
  2. with that worked around: `No such method 'usage-name' for invocant of type
     'Parameter'` (LinkGenerator.rakumod:66)
  Measured 2026-08-09 with a `*@a`-substituted copy of the test plus a shadowed
  LinkGenerator; the file is unmeasured beyond `usage-name`.

## Repro

Verified against release binary vs raku:

```
$ raku  -e 'sub f("x") {}; say &f.signature.params[0].constraint_list.raku'
("x",)
$ mutsu -e 'sub f("x") {}; say &f.signature.params[0].constraint_list.raku'
No such method 'constraint_list' for invocant of type 'Parameter'

$ raku  -e 'sub f($x, :$y) {}; say &f.signature.params[0].usage-name'
x
$ mutsu -e '...same...'
No such method 'usage-name' for invocant of type 'Parameter'

$ raku  -e 'sub f($x) {}; say &f.signature.params[0].default.raku'
Code            # type object when the parameter has no default
$ mutsu -e '...same...'
No such method 'default' for invocant of type 'Parameter'
```

Working already: `.name`, `.named`, `.positional`, `.optional`, `.slurpy`,
`.type`, `.constraints` (returns the `all(...)` junction), `.named_names`.

Cro usage (lib/Cro/HTTP/Router/LinkGenerator.rakumod, `signature-to-sub`):
`$p.constraint_list == 1 && $p.constraint_list[0] ~~ Str` (line 25-27),
`$param.usage-name` (line 66), `@default.push: $_ with $param.default` (for
optional params). Router.rakumod itself avoids `constraint_list` via an
autothreading `extract($param.constraints)` helper, which mutsu handles.

## Root cause

mutsu builds Parameter objects as plain Instances whose methods are attribute
lookups: `make_param_attrs` in src/value/signature.rs (~lines 300-464).

- `constraints` is inserted as an `all(...)` junction (signature.rs:441-455) but
  no `constraint_list` attr/method exists. In Rakudo `constraint_list` is the
  underlying List of the same items.
- `usage-name` is never inserted (it is the variable name minus sigil and
  twigil: `@foo` → `foo`, `$!x` → `x`).
- `default` is inserted only when the parameter HAS a default
  (signature.rs:420-433); an absent attribute makes method dispatch fail with
  "No such method" instead of returning an undefined value. Rakudo returns the
  `Code` type object when there is no default.

## Fix direction

All in `make_param_attrs` (src/value/signature.rs), where `constraint_items`
is already computed (:441-451):

1. `attrs.insert("constraint_list", Value::array(constraint_items.clone()))`
   — insert BEFORE the junction consumes the Vec; `.raku` should render as a
   List (check how mutsu prints `Value::array` under `.raku` — raku prints
   `("x",)`; if array prints `["x"]`, use the list/List constructor instead).
2. `usage-name`: compute from the already-known name — strip leading sigil
   (`$@%&`) and twigil (`!.*^:?`) — and insert as Str. There may be an existing
   helper for this near `parameter_to_raku` (signature.rs:467).
3. `default`: when `p.default_expr` is None, insert the `Code` type object
   (`Value::Package(Symbol::intern("Code"))` or whatever mutsu uses for type
   objects) so `with $param.default` is False-y but the method exists. Verify
   `raku -e 'sub f($x) {}; say &f.signature.params[0].default.defined'` → False
   and match it.

Alternatively, if Parameter has a native-method dispatch table somewhere,
adding methods there is fine too — but the attrs map is how the existing
methods (`named`, `optional`, ...) are served, so extending it is the
consistent move.

Risk: low; additive. Watch `.raku`/`.gist` roast pins for Parameter
(`S06-signature/introspection*.t` if whitelisted) since new attrs may leak into
introspection output if `parameter_to_raku` iterates attrs (it reads specific
keys, so it should be safe — confirm).

## Verification

- The three repro one-liners match raku.
- `t/http-router-named-urls.t` (with the `+a` parser fix): `signature-to-sub`
  completes and the file starts emitting TAP. **The file is unmeasured past
  `usage-name`** — expect possibly more gaps (the same sub also uses
  `$param.type`, `$param.positional`, `$param.named`, `$param.slurpy`,
  `$param.optional`, all currently working standalone). Re-run and triage
  whatever surfaces next.
- Roast: `S06-signature/introspection.t` if runnable, plus a `t/` pin
  `t/parameter-introspection.t` covering the three methods incl. literal
  (`"x"`), where-clause, and no-constraint parameters.
