# `$obj!Class::method` is rejected when the class lives inside a module

Reduced 2026-08-19 while working the `Template::Jinja2` row of
[todo/deep/template-engines-blocked-on-mutsu.md](../deep/template-engines-blocked-on-mutsu.md).
This is the blocker that stands **behind** the char-class bug: with
[regex-brace-paren-inside-char-class-swallows-rest-of-pattern.md](regex-brace-paren-inside-char-class-swallows-rest-of-pattern.md)
worked around by hand, 22 of the dist's 23 test files still die at module-load
time on this one.

## Repro

```raku
module Outer::Inner {
    class Renderer is export {
        method !secret($x) { "secret:$x" }
        method go($x) {
            my $r = self;
            return $r!Renderer::secret($x);
        }
    }
}
import Outer::Inner;
say Renderer.new.go(42);
```

raku prints `secret:42`. mutsu dies before running anything:

```
Cannot call private method without permission
  in block <unit> at tmp/priv3.raku line 2
```

Two controls pin the cause exactly:

- Drop the enclosing `module` (a bare top-level `class Renderer { … $r!Renderer::secret(…) }`)
  and mutsu is **fine**.
- Keep the module but write the owner fully qualified
  (`$r!Outer::Inner::Renderer::secret($x)`) and mutsu is **fine**.

## Root cause

`validate_private_access_in_expr()` in `src/runtime/registration.rs` (~line 518)
validates `$obj!Owner::meth` statically, at class-registration time:

```rust
if *modifier == Some('!')
    && let Some((owner_class, _)) = name.resolve().rsplit_once("::")
    && owner_class != caller_class
    && !self.registry().class_trusts.get(owner_class)
        .is_some_and(|trusted| trusted.contains(caller_class))
{
    return Err(RuntimeError::typed_msg("X::Method::Private::Permission", …));
}
```

`owner_class` is taken verbatim from the source text — the *short* name the
programmer wrote (`Renderer`) — while `caller_class` is the class's
**fully-qualified** registered name (`Outer::Inner::Renderer`). The comparison is
a plain string equality, so the two never match and the check false-positives on
what is a perfectly legal self-call. The `class_trusts` lookup is keyed on the
raw `owner_class` too, so the `trusts` escape hatch cannot rescue it either.

Raku resolves `Renderer` in `$r!Renderer::secret` through the ordinary package
lookup of the enclosing lexical scope, which inside `module Outer::Inner` finds
`Outer::Inner::Renderer` — the same class — so access is granted.

## Impact — measured

`Template::Jinja2` 0.2.0's `lib/Template/Jinja2/Renderer.rakumod` is
`module Template::Jinja2::Renderer { … class Renderer is export { … } }` and
calls `$renderer!Renderer::render-block-with-super(…)`,
`$renderer!Renderer::render-for-items(…)`, `$renderer!Renderer::eval(…)` and
`$renderer!Renderer::render-body(…)` from closures inside the class body (5 call
sites). `use Template::Jinja2::Renderer;` alone is enough to reproduce, so the
whole dist is unloadable and 22 of its 23 files die before their first assertion.

This is **not** the private-method-in-closure bug fixed in #5466
(`news/2026-07/private-method-in-closure.md`) — the closure form from that fix
works now; this is the *qualified* form with a short owner name, and it is a
different code path (a static registration-time check, not a dispatch-time one).

## Proposed fix

Resolve the owner name to the same canonical class identity the registry uses
before comparing, instead of comparing raw source text:

1. Canonicalise `owner_class` through the package resolution already used when
   registering / looking up a class — the enclosing package chain of
   `caller_class` first, then the global registry. In the repro, `Renderer`
   written inside `Outer::Inner::Renderer` must resolve to
   `Outer::Inner::Renderer`.
2. Compare the *canonical* names, and key the `class_trusts` lookup on the
   canonical owner name too, so `trusts` keeps working for the short form.
3. If the owner name does not resolve to any registered class, keep today's
   behaviour (reject) rather than silently allowing — an unresolvable owner is
   still an error, just not this one.

The cheap, obviously-correct shape of step 1 is "accept when `caller_class` ==
`owner_class`, or when `caller_class` ends with `::` + `owner_class`", but do the
real resolution rather than a suffix test: a suffix test would wrongly accept
`A::B::Renderer` calling `$x!Renderer::…` where `Renderer` lexically names an
unrelated top-level class.

## Pin to add

`t/private-method-qualified-short-owner.t` — the repro above, plus:

- the fully-qualified spelling (must keep working),
- the bare top-level class (must keep working),
- a genuine violation (`class A { method !s {…} }; class B { method go($a) { $a!A::s } }`)
  which must still throw `X::Method::Private::Permission` (verified: mutsu
  rejects this today, and must keep rejecting it),
- the same pair with `class A { trusts B; … }`, which must be allowed (verified:
  mutsu allows it today) — including when the owner is written short inside a
  module.

While in this function it is worth upgrading the message, too: raku says
`Cannot call private method 's' on package 'A' because it does not trust the 'B'
package.` where mutsu says only `Cannot call private method without permission`,
which is what made this failure so opaque inside a 900-line module.

## Affected files

- `src/runtime/registration.rs` — `validate_private_access_in_expr()` and the
  `class_trusts` lookup beside it.
