# Air::Plugin::Donate goes green, and takes five general interpreter fixes with it

`Air::Plugin::Donate` 0.0.1 is a 45-line Stripe donation panel for the `Air` web framework: one
role, one `HTML` method, one test file that rakudo passes 17/17. mutsu passed 1 of those 17 and died
on the second, and the ledger recorded it as `red`.

It turned out to be five independent interpreter gaps stacked one behind the other, each of which
only became visible once the one in front of it was fixed. None of the five is specific to the
distribution, and one of them was reproducible with no roles or coercions at all.

## 1. A role pun kept both copies of a shadowed multi candidate

The plugin is a role that composes `Air::Functional`'s `Tag` role and overrides its `multi method
HTML`, and it is instantiated as a *pun* (`Air::Plugin::Donate.new`). Every call died with:

```
Ambiguous call to 'HTML(Air::Plugin::Donate: )'; these signatures all match:
  (Air::Plugin::Donate $:: *%_)
  (Air::Plugin::Donate $:: Singular $__type_only__, *%_)
  (Air::Plugin::Donate $:: Regular $__type_only__, *%_)
  (Air::Plugin::Donate $:: *%_)
```

The same signature, printed twice. Rakudo's role-to-role applier resolves this when `role D does B`
is compiled: `B`'s candidates are flattened into `D` with `D`'s own winning per signature, so `D`
ends up holding one `HTML()` (its own) plus `B`'s other two. mutsu appended the parent's candidates
verbatim and left each *consumer* to notice the duplicate. `class K does D` did notice, in
`resolve_class_stub_requirements` — but a pun builds its method table by a different route and never
reached that pass.

Fixed at the one canonical point instead of in a second consumer: `finish_role_registration` now
prunes the shadowed candidates from the role's own method table, so `roles[name].methods` is correct
for the pun, for mixin dispatch and for class composition alike.

## 2. `OUR::` pseudo-stash bindings published nothing

`Air::Functional` exports one sub per HTML tag — `h3`, `p`, `article`, `ul`, ~100 of them — by
generating them into its own export stash:

```raku
my package EXPORT::DEFAULT {
    for @regular-tags -> $tag {
        OUR::{'&' ~ $tag} := sub (*@inners, *%h) { do-regular-tag("$tag", @inners, |%h) }
    }
}
```

`use Air::Functional` imported none of them: every tag was `Unknown function`.

`OUR::` names the *current* package, so such a binding is that package's own symbol — and its
module's export when the package is an `EXPORT::<tag>` stash. mutsu stored it under the literal env
key `&OUR::name`, which only the identical spelling could read back; `Foo::hi()` answered "Could not
find symbol '&hi' in 'Foo'" and the scalar form `OUR::<$x> := 1` was simply lost, because a read
resolves against the current package and so never looked where the write had landed. The
runtime-key spelling did not even reach that: the compiler only handled a literal subscript, and the
generic index-assign it fell through to writes a throwaway stash hash.

These are fresh closures over the loop variable, so the existing `register_our_code_alias` — which
aliases an *existing* named routine by copying its `FunctionDef` — could not serve them: there is no
`FunctionDef` to find, and the captured `$tag` is exactly what must survive. The closure value
itself is published now, as the module's exported symbol, in the same representation an `our &f is
export = sub { ... }` already used.

## 3. An imported code variable was invisible to the importing compunit's own routines

With the tag subs finally imported, they worked from a script's mainline and were `Unknown function`
from inside `Air::Plugin::Donate`'s role method. This one had nothing to do with `OUR::`: plain
`our &f is export` reproduced it.

An imported code variable is a lexical of the importing compunit, and the module load restores the
caller's scope over its `env` entry. The surviving record is `module_scope_lexicals`, keyed by the
packages that compunit declared — which is what `module_scope_lexical` already serves for a module's
own bare `constant`s and sigilless declarations. The two call paths for a bare `&name` callable now
consult it as well, so the name resolves from a `sub`, a class method and a role method of that file,
and stays invisible to a compunit that never imported it.

## 4. Coercion into a role that inherits a built-in type

Every rendered tag comes back through `--> Markup()`, where `role Markup is Str is
export(:MANDATORY) {}`. mutsu had no answer for that: the role declares no `CALL-ME`, `COERCE` or
`new`, so the coercion died with `X::Coerce::Impossible`.

Such a role says "a Str that also does Markup", and mutsu already represents exactly that — a
non-`Instance` value carries roles in the mixin wrapper, because an `Int`/`Str` has no shared
attribute node to rebless (`types::role_mixin_class`'s own doc comment). So the coercion is the role
composed onto the value once the value has been coerced to the built-in parent.

The **class** case (`class C is Str {}; C('x')`) is deliberately not handled: it needs a
built-in-backed instance representation mutsu does not have, since an instance of such a class today
carries no payload at all (`CM.new.Str` is `""` by fallback, and `.^attributes` is empty). Filed as
[#8856](https://github.com/tokuhirom/mutsu/issues/8856), and named from the two code comments and the
regression test that stop short of it.

That fix immediately exposed a second one, in multi dispatch. `Air::Functional` renders through
`multi render-tag(Markup $)` and HTML-escapes through `multi render-tag(Str() $)`; mutsu picked the
escaping candidate and mangled its own markup into `&lt;h3&gt;`. A value carrying mixed-in roles sits
one step *below* its inner type, exactly as rakudo's synthesized `Str+{Markup}` class does, but
`type_hierarchy_distance` scored the inner type at 0 and the role at the 500 "unrelated" distance, so
the wider candidate won every tie-break. Measured against rakudo in both directions first: with a
`role M is Str`, rakudo picks `M` over `Str`; with a role that inherits nothing, rakudo calls the
same pair *ambiguous*, which is a different (and much narrower) divergence.

## 5. An imported short name did not resolve as a coercion target from a routine

The plugin writes `--> Markup()` in its own methods, where `Markup` is a short name imported from
`Air::Functional`. From a routine declared in the importing compunit, the coercion's alias lookup
found nothing and left the constraint as an unregistered short name.

The tell was that `Markup ~~ Str` answered `True` from the very same frame: type *matching* already
consulted `package_type_alias`, the surviving per-package record of the import, and only
`resolve_constraint_alias` did not. It does now.

## 6. `bless` did not enforce `is required`

The last assertion is `throws-like { Air::Plugin::Donate.new }, Exception, 'key is required'`, over a
role declaring `has Str $.key is required` alongside `multi method new(*%h) { self.bless: |%h }`.
mutsu constructed the object happily.

Rakudo enforces `is required` in `BUILDALL`, which `bless` runs; mutsu checked it only on the default
`new` path, so the commonest custom-constructor idiom — a `method new` that delegates to `bless` —
skipped the check entirely, whether the attribute came from the class or from a composed role. A
*typed* required attribute is what hid it: the pre-BUILD seed leaves the declared type object in the
slot, and the existing check only recognised `Any` as unset.

## Result

`Air::Plugin::Donate` 0.0.1: 1/1 baseline files, 17/17 assertions, `red` -> `green`.

Each fix is pinned by its own test, all five verified against `raku` as the oracle:

- `t/oo/role/role-parent-multi-shadowed-on-pun.t`
- `t/modules/import-export/our-stash-generated-exports.t`
- `t/oo/role/coerce-into-role-inheriting-builtin.t`
- `t/modules/import-export/imported-type-name-in-coercion.t`
- `t/oo/construct/bless-enforces-is-required.t`
