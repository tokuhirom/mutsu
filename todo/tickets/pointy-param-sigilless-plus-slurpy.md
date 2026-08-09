# Pointy-block signatures reject the sigilless one-arg slurpy `+a`

## Affected tests

- `t/http-router-named-urls.t` (Cro::HTTP dist) — produces NO TAP at all.
  Compilation dies:
  `===SORRY!=== ... Confused. expected statement ... at t/http-router-named-urls.t:104`
  pointing at `test-route-urls route {`. The actual offender is INSIDE that
  block, line 110: `get :name<css>, -> 'css', +a { };` — the `+a` parameter.
  raku compiles and runs the file.

## Repro

Verified 2026-08-09 (release binary):

```
$ mutsu -e 'my $b = -> $x, +a { say a.raku }; $b(1,2,3)'
Runtime error: X::Syntax::Malformed: Malformed initializer
$ mutsu -e 'my $b = -> +a { say a.raku }; $b(1,2,3)'
Runtime error: X::Syntax::Malformed: Malformed initializer
$ raku  -e 'my $b = -> $x, +a { say a.raku }; $b(1,2,3)'
(2, 3)
```

The SUB path already works (different parser module):

```
$ mutsu -e 'sub f($x, +a) { say a.raku }; f(1,2,3)'
[2, 3]
```

(Sigiled forms `+@a`, `+$a`, `*@a` parse fine in pointy blocks too — only the
sigilless `+name` form fails.)

## Root cause

Pointy-block parameters are parsed by `parse_pointy_param`
(src/parser/stmt/control/pointy_param.rs:3). Its slurpy-marker section
(pointy_param.rs:112-137) handles `**`-double-slurpy and `*`/`+` followed by a
sigil (`@ % $ &`) only. For `+a` no branch strips the `+`, so control falls to
`var_name(rest)` (pointy_param.rs:388) which fails on `+`, the whole
`->`-lambda parse fails, and the statement parser surfaces "Malformed
initializer" (assignment context) or "Confused" (file context, reported at the
statement's first line — which is why the file error points at line 104, not
110).

The sub-signature parser has the missing branch:
src/parser/stmt/sub_param/param_inner.rs:158-194 ("Sigilless single-argument
rule slurpy: +foo") — parses the ident, optional `is` traits and default, and
returns a ParamDef with `slurpy = true; onearg = true; sigilless = true`.

## Fix direction

In `parse_pointy_param` (src/parser/stmt/control/pointy_param.rs), after the
existing `+`-with-sigil branch (:129-137), add the sigilless variant mirroring
param_inner.rs:158-194:

- condition: `rest.starts_with('+') && rest.len() > 1 &&
  (rest.as_bytes()[1].is_ascii_alphabetic() || rest.as_bytes()[1] == b'_')`
- parse `ident`, then reuse the function's existing trait/default tail handling
  (or return early like param_inner does), and return a ParamDef with
  `slurpy: true, onearg: true, sigilless: true, block_param: true`.

The lambda body compilation already supports sigilless params
(`parse_block_body_with_sigilless` / `register_sigilless_params` in
src/parser/primary/misc/lambda.rs:399-452 key off `ParamDef.sigilless`), so no
change should be needed there — but verify the body can reference bare `a`.

Note while testing: the sub path prints `[2, 3]` where raku prints `(2, 3)` —
the `+a` binding produces an Array-flavored gist instead of a List. Cosmetic
for this ticket, but worth a pin if cheap to align.

Risk: `+` is also a prefix operator; the new branch must require the alphabetic
first char (as above) so `-> +$a`/`-> +@a` stay in the sigiled branch and
expression-position `+foo` (not a signature) is unaffected — the branch only
runs inside pointy-param context, so blast radius is small.

## Verification

- The three repro one-liners behave like raku (binding: `a` = remaining args
  under the one-arg rule).
- `t/http-router-named-urls.t` gets past compilation. **The file then hits the
  next blocker immediately** (measured 2026-08-09 by substituting `*@a` for
  `+a` in a copy): `No such method 'usage-name' for invocant of type
  'Parameter'` (and behind it `constraint_list`, `default`) inside
  `Cro::HTTP::Router::LinkGenerator::signature-to-sub` — see ticket
  `tap-parameter-introspection-methods-missing.md`. The file remains unmeasured
  beyond `route`-block registration until that lands.
- Add `t/pointy-onearg-slurpy.t`: `-> +a {}`, `-> $x, +a {}`, `-> 'lit', +a {}`
  (the Cro shape), with one-arg-rule flattening checks against `raku -e`.
