# Fixed: qualified private method calls with a short owner name inside a `module`

`$obj!Owner::method` — a private-method call whose owner is written short in
source (`Renderer`) rather than fully qualified
(`Outer::Inner::Renderer`) — was wrongly rejected with "Cannot call private
method without permission" whenever `Owner` lived inside a `module`. The
static permission check in `validate_private_access_in_expr()`
(`src/runtime/registration.rs`) string-compared the raw short owner name
parsed from source against the caller's fully-qualified registered name, so
even a perfectly legal self-call written from inside a `module` (`class
Renderer` declared inside `module Outer::Inner`, calling
`$r!Renderer::secret(...)` on itself) false-positived at class-registration
time.

Fixed by canonicalizing the owner name through the same package-chain
resolution ordinary bareword type references use (walking the caller's
enclosing packages outward, then falling back to a direct global lookup)
before comparing it against the caller's class or looking it up in
`class_trusts` — never by a raw suffix/substring test, which would wrongly
accept an unrelated top-level class that happens to share the short name.

The exact same short-owner-name bug existed independently in four *runtime
dispatch-time* permission checks (`methods_qualified.rs`,
`methods_instance_ops.rs` ×2, `methods_signature_shaped.rs`,
`methods_mut_method_lvalue.rs`) — copies of the same logic that also used
the raw short owner name both for the trust check and, in three of them, for
the actual method-resolution MRO lookup (`"A" != "M::A"` never matches a
fully qualified MRO entry, so passing a short owner also broke *finding*
the private method, not just the permission check). All five were unified
onto two new shared helpers on `Interpreter`:
`resolve_private_class_name()` (the canonicalization) and
`private_owner_trusts_caller()` / `resolve_and_check_private_owner()` (the
trust check, itself also canonicalizing each `trusts` entry the same way —
so `trusts B;` written inside a `module` grants access to the
module-qualified `B`, not just the literal source text `"B"`).

As a side effect, the permission-denied error now carries the same
structured `X::Method::Private::Permission` shape (`method`,
`source-package`, `calling-package` attributes) and Rakudo-matching message
(`Cannot call private method 'foo' on package 'A' because it does not trust
the 'B' package.`) that some of the five call sites already produced but
others did not.

This was the blocker standing behind the char-class fix
(`news/2026-08/regex-char-class-literal-brace-paren.md`) for
`Template::Jinja2`'s test suite: `lib/Template/Jinja2/Renderer.rakumod` is
`module Template::Jinja2::Renderer { class Renderer is export { ... } }`
calling `$renderer!Renderer::render-block-with-super(...)` and similar from
closures inside the class body, so `use Template::Jinja2::Renderer;` alone
died before this fix.

Pinned by `t/private-method-qualified-short-owner.t` (module-nested
self-call, fully-qualified spelling, bare top-level owner, a genuine
cross-class violation that must still be rejected, top-level `trusts`, and
module-nested `trusts` with both a short and a fully-qualified owner name).
