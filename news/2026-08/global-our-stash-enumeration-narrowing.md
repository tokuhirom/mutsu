# `GLOBAL::`/`OUR::` stash enumeration no longer leaks builtin/dynamic noise

`GLOBAL::.keys` and `OUR::.keys` at file scope (the same stash --
`our_pseudo_stash()` is `package_stash_value(current_package)`, which is
`GLOBAL` at file scope) used to return every registered builtin class
(`Promise`, `Int`, `Thread`, `Backtrace`, ... around 40 of them), every dynamic
variable (`$*CWD`, `%*ENV`, `$?FILE`, `$=pod`, ...), and even `my`-scoped
lexicals that merely happened to live in the same flat env store as real
package symbols. Rakudo returns only the package's own `our` declarations and
sub-packages -- a bare `our $o = 2;` at file scope gives `GLOBAL::.keys ==
($o)`, nothing else.

## Measuring the ticket's deferred blocker first

The ticket this closes (`our-pseudopackage-missing-file-scope-symbols.md`)
deferred the fix because narrowing `package_stash_value` changes `GLOBAL::`
*membership*, and the same map serves both `.keys` enumeration and the
symbolic-deref lookup road (`::('Name')`, `GLOBAL::<Name>`). The ticket flagged
a promising signal to check first: `t/bare-package-symbolic-deref.t` already
asserts `dies-ok { GLOBAL::('Int') }`, i.e. builtin types are not supposed to
be reachable through the `GLOBAL` stash at all.

That signal held up. Reading each lookup road that reaches
`package_stash_value` showed the coupling is much shallower than it looked:

- `GLOBAL::<Int>` (angle-bracket subscript on a pseudo-stash) does not even
  reach `package_stash_value` -- the parser treats `Pkg::<Ident>` as an
  ordinary qualified bareword (`GLOBAL::Int`), resolved through
  `resolve_indirect_type_name`'s own type-recognition logic, not through
  stash-symbol lookup.
- `::('GLOBAL::Foo')`/`::('GLOBAL::Int')` (`resolve_indirect_type_name`'s
  split-on-`::` walk) already failed with "No such symbol" *before* this
  change, for both builtin and user-declared names, because it never
  recognizes `GLOBAL` as a resolvable first path segment in the first place --
  a distinct, pre-existing gap unrelated to stash membership.
- `GLOBAL::{"Int"}` (postcircumfix curly subscript) does reach
  `package_stash_value`, and rakudo itself resolves it to `(Any)` (undefined),
  not the `Int` type object -- so narrowing membership does not regress
  anything real raku already supports there either.

So no live lookup path actually depends on builtin classes or dynamic
variables being *members* of the `GLOBAL` stash. The over-broad enumeration
was purely a `.keys`/`.gist`-visible artifact of two loops in
`package_stash_value` (`src/runtime/accessors_stash.rs`) treating every
registered class and every env entry as a `GLOBAL` member unconditionally
(`stash_member_tail` returns the full key for `GLOBAL` with no prefix to
strip, unlike a named package).

## The fix

- **Classes/roles loops**: for `package_name == "GLOBAL"`, only classes in
  `self.user_declared_classes` and roles in
  `self.registry().user_declared_roles` are members -- both sets already
  exist and are populated at every `class`/`package`/`module`/`grammar`/`role`
  declaration, so no new bookkeeping was needed (the ticket's guess that
  "classes have no user-declared marker" was wrong; `user_declared_classes`
  already existed, just unused for this purpose).
- **Env scan loop**: added `is_global_root_symbol`, which keeps sigiled
  array/hash/scalar keys (`@arr`, `%h`, `$x`) and uppercase bare names (types,
  constants, enum members -- always visible from the enclosing package) but
  rejects dynamic variables (`*CWD`/`$*CWD` -- some dynamic vars are mirrored
  into env both bare and pre-sigiled), compile-time magicals (`?FILE`, `=pod`),
  internal bookkeeping keys (`__mutsu_*`), and lowercase bare names (`my`
  lexicals -- genuine `our` scalars are already covered by the dedicated
  `our_vars` loop). A known-builtin type name mirrored directly into env
  (`runtime_init.rs`'s `env.insert("Any", ...)` sentinel) is excluded via the
  existing `is_known_type_constraint` utility, not a new hardcoded name list.
- **Self-qualification stripping**: a root-scope `our $o` is mirrored into
  both `our_vars` and `env` twice -- once bare (`o`) and once qualified with
  the current package (`GLOBAL::o`). Before this fix the qualified form was
  misread as a sub-package named literally "GLOBAL"; both loops now strip a
  leading `GLOBAL::` self-prefix before deciding what kind of member a key
  names, so `GLOBAL::o` collapses onto the same `$o` entry the bare mirror
  already produces instead of adding phantom `GLOBAL` / `Mod::modvar`-style
  flat keys.

Verified against rakudo v2026.06 across file scope, inside `package`/`module`
blocks, with `my` vs `our` declarations, classes/roles/enums/constants, and
qualified declarations (`my $foo::bar = 1`) creating implicit sub-packages --
`GLOBAL::.keys` and `OUR::.keys` now match exactly in every case tried.
`t/bare-package-symbolic-deref.t` stays green (still 10/10), and
`roast/6.c/S02-names/pseudo-6c.t` still fails exactly its recorded baseline
(14/161, an unrelated CALLER/stash-road cluster).

## What was *not* in scope

While measuring, `our sub baz {}` at file scope was found to not expose a
`&baz` member in `GLOBAL::`/`OUR::` at all (rakudo does). This is a separate,
pre-existing *under*-inclusion bug in the `registry().functions` scan, not
caused by (or fixed by) this change; it is unrelated to the over-broad-noise
problem this entry closes and is tracked separately.

## Tests

Extended `t/eval-compunit-introspection.t`'s existing "OUR:: exposes the
current package's own symbols" section with 15 new assertions covering
inclusion (root `our` scalar, user class/role/enum/constant) and exclusion
(`my` lexical, builtin types, dynamic vars, compile-time magicals, internal
keys), agreement between `GLOBAL::` and `OUR::` at file scope, and correct
package-relative scoping inside a named `package` block. All 62 assertions
pass verbatim under both `raku` and `mutsu`.
