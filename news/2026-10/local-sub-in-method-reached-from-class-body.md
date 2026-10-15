# A `my sub` in a method stays callable from the method's closures when the method runs under another package

A `my sub` declared in a method body is registered in the routine registry under the package that
is current when the method runs. Method dispatch does not re-point the current package, so a method
called from a class body (an attribute trait handler, a `BEGIN`) registered the helper under the
caller's package, while a `method`/`anon method` literal made in that body runs under its own
class and looked the helper up there: `Unknown function: gs`. Pointy blocks were unaffected
because they run under the caller's package.

The block-lexical escape hatch (the helper's Sub stored in the closure's env, read after a registry
miss) was only armed inside a block scope, never inside a routine body. It is now armed inside
a routine too, so the lookup falls back to the captured Sub.

This was the first blocker of AttrX::Mooish (#13264): the installed `clearer`/`predicate` helper
methods call `get-attr-obj`, a `my sub` of `setup-attr-helpers`. `$f.has-bar` now answers.
The remaining blockers of that distribution (a HOW-owned `@!BUILDPLAN`, role methods composed into
the custom HOW) are recorded on #13264.

Pinned in `t/oo/method/local-sub-in-method-reached-from-class-body.t`.
