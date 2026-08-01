# A bare type name resolves through its enclosing packages in call position too

A `class` or `role` declared under a non-`GLOBAL` package registers
package-qualified — `M::C` for a `class C` inside `module M` — which is what
raku does as well. mutsu already resolved a *bare* reference to it in
type-object position by walking outward through the enclosing packages
(`bare_name_packages`, the same chain bare *routine* lookup uses). Two other
positions did not:

- **Call position.** `C("x")` is a coercion and `99 but R("x")` initializes a
  role's single public attribute; both key the registry by the name as written,
  so from inside `M` they died with `Unknown function: C`.
- **`augment`.** `augment class C` / `augment role R` inside `M` looked up the
  bare name, found nothing, and reported `X::Augment::NoSuchType` — for a type
  that exists and, being a role, should have earned
  `X::Syntax::Augment::Illegal` instead.

Both now go through one `resolve_bare_type_name` helper.

## The chain is consulted before the unqualified name

Innermost package first, unqualified last. That is raku's precedence for a
lexically inner declaration, and getting it backwards is not academic: with the
unqualified key checked first, a stale global `R` left in the registry by an
*earlier* `EVAL` in the same process shadowed the `M::R` the current unit had
just declared, so `throws-like 'my role R { }; 99 but R("wrong")',
X::Role::Initialization` quietly succeeded against the previous test's role
instead of failing against this one. The first draft of this change had exactly
that bug and the vendored-`Test` sweep caught it.

## Effect

Found through the Test-vendoring sweep (`todo/tickets/vendor-real-test-module.md`),
where the calling module is `Test` itself: `EVAL $code, context => ...` from
inside `Test.rakumod` runs the snippet under package `Test`, so a snippet that
declares its own type could not then refer to it. Four `t/` files go green under
the aliased upstream module — `role-initialization.t`, `augment-role-anon.t`,
`augment-nosuchtype.t`, `eval-type-decl-and-phaser-message.t`.

The other half of that problem — `.package-name` reporting `Test2::Foo` where
raku reports `Foo`, which needs `EVAL`'s `context` argument to be honoured — is
still open in `todo/deep/eval-context-argument-is-ignored.md`.

Pinned by `t/bare-type-name-under-a-package.t`, whose 7 assertions are green
under `raku` too.
