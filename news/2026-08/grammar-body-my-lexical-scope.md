# Grammar-body `my` lexicals are now visible to token/rule dispatch

A `my` variable declared directly in a `grammar` body (like a class-body static — see
`t/class-body-my-lexical-scope.t`) is a lexical of that body: a `token`/`rule` method
should see it the same way an ordinary method sees a class-body static.

mutsu's class-body-static injection (`inject_class_body_statics`) only ran on the two
method-dispatch paths, never on grammar token/subrule dispatch. A pattern that
interpolates such a variable (`@array`/`$scalar`/`%hash`) silently resolved it to `Nil`
at match time instead of its declared value, instead of raising an error — since an
interpolated `Nil` in a regex alternation position simply matches nothing, the token
would just always fail to match that part of its pattern.

Discovered via the vendored `Cro::HTTP::Cookie` module's
`token cookie-av:sym<samesite> { :i 'SameSite=' @same-site-opts }`, where
`@same-site-opts` is a grammar-body `my @same-site-opts = SameSite.enums.values;`.
Real `SameSite=Strict`/`Lax`/`None` cookie attributes were silently dropped on
round-trip through `Cro::HTTP::Cookie.from-set-cookie`/`.to-set-cookie` — the
`samesite` token candidate could never match, so proto dispatch always fell through to
the generic `cookie-av:sym<extension>` catch-all instead.

Fixed by adding `establish_grammar_body_statics` (mirroring the existing
`establish_grammar_dynamic_vars` for `:my $*/%*/@*NAME` dynamic variables), invoked
both where a grammar's own `TOP`/named-rule dispatch begins and where a token/rule body
is re-parsed on each match attempt (`regex_token_resolve.rs`). Also fixed a related
subrule-catch-all vs. array-interpolated-candidate tie-break: once the array
resolves correctly, an array-interpolated proto-token candidate now correctly wins an
LTM tie against a sibling candidate that calls a **named subrule** catch-all — pin:
`t/grammar-body-my-lexical-scope.t`.

A narrower, separate LTM discrepancy remains when the sibling catch-all is an
**inline** unbounded quantifier (`<-[;]>+` written directly in the token body, not via
a named subrule) — recorded as `todo/deep/ltm-inline-unbounded-quantifier-vs-array-tie.md`
since it needs real Rakudo LTM-algorithm research, not a quick patch, and no known
roast test currently depends on that exact shape.

## Effect

- `roast/packages`-vendored Cro::HTTP suite: `http-cookie.rakutest` and
  `http-cookiejar.rakutest` SameSite round-trip subtests now pass.
