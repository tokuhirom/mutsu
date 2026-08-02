# A plain `sub` in a module is not reachable under its package name

In Raku a routine declared with a bare `sub` is `my`-scoped, whatever package
body it sits in. `is export` makes it importable under its short name; it does
**not** put it in the package stash. Only `our sub` does that:

```
unit module MScope;
sub       lex-sub()   is export { "lex" }
multi sub multi-lex(Int $x) is export { "multi-lex $x" }
our sub   pkg-sub()   is export { "pkg" }
```

```
$ raku -I. -e 'use MScope; say MScope::pkg-sub(); say MScope::lex-sub()'
pkg
Could not find symbol '&lex-sub' in 'MScope'
```

mutsu answered `lex` for the second one, and `multi-lex 1` for the multi. Two
independent leaks, one per shape.

## The qualified-call retry fired for a package mutsu knows

`call_function_fallback` ends with a package-prefix strip: an unresolved
`Foo::bar(…)` retries as `bar(…)`. That retry exists so a call qualified with a
package mutsu never registered still finds its routine, and
`news/2026-07/qualified-call-no-longer-aliases-a-builtin.md` already gated it on
*something being declared* under the short name — which is exactly the case
here, since the routine really is imported. The missing half of the gate is the
qualifier: when the package is one mutsu **has** registered, the qualified name
already had its chance through the registry, and stripping the prefix
resurrects every lexical routine of every loaded module. The retry now runs
only for an unknown package — the same `is_known_package` predicate that
already decided whether the error names `Foo::Bar` or `GLOBAL::Foo::Bar`.

## The my-scoped gate sat below the multi candidate scan

`multi sub` survived even that, because the visibility gate was attached to the
wrong lookup. `dispatch_resolve` checked `is_my_scoped_package_item` on the
exact-name registry hit — but a multi is registered under `Pkg::name/arity`
keys, so the exact-name lookup misses by construction and control fell through
to the arity-keyed candidate scan, which had no gate at all. It handed back the
very routine the check above had been written to hide. The gate is now the
first thing the qualified branch does, so every lookup below it inherits the
decision (and the duplicated copy further down became redundant and was
removed).

While moving it, the in-package exemption went too: it let `M::lex-sub()`
resolve from *inside* `M`. Raku does not — a lexical routine is not in the
stash for anyone, so the qualified form fails there as well, which the pin
asserts under both implementations.

## Where it came from

`t/qualified-call-does-not-alias-builtin.t` asserts that `Test::ok(1)` dies. It
was passing under mutsu's native `Test` provider and failing under the vendored
upstream module (`todo/tickets/vendor-real-test-module.md`), where `Test` is a
real module with a real `multi sub ok`. It had been filed as "the pin asserts
native-provider behaviour, re-point the test file". It was not: `raku -e 'use
Test; Test::ok(1, "q")'` says `Could not find symbol '&ok' in 'Test'` too,
because rakudo's `Test.rakumod` declares `multi sub ok(...) is export` — a
lexical. The pin was right and mutsu was wrong.

Pin: `t/module-sub-package-visibility.t` (+ fixture `t/lib/PackageSubScope.rakumod`),
passing unchanged under `raku`. `make test` and the bundled-library gate
(`scripts/battery-testsuite.sh`, 153/158) are green. This takes the real-`Test`
regression ledger from 13 to 12.
