# A package-qualified type name is a routine trait (`is Path::Map(...)`)

`sub (:$foo!) is Path::Map(:test<foo/:foo>) { ... }` — the idiom the
`Path::Map` distribution documents for registering a handler — failed to parse
with the generic `Confused. expected statement: expected expression statement or
anonymous sub parameter list/body or '{'` message, one of the undescribed
members of the #7988 parse-gap cluster. The routine-trait parser read the name
after `is` with the plain identifier scanner, which stops at `::`, so the rest
of the qualified name was left behind as garbage in front of the body.

`parse_sub_traits` now reads the trait name as a package-qualified identifier
(each `::` must be followed by another segment). The runtime side already
resolved a qualified name through `resolve_type_object`, so a qualified type
trait dispatches `trait_mod:<is>` with the type object as a positional, exactly
like `is Foo(...)`.

Pinning that down exposed a second, independent bug on named subs: the
parenthesized trait argument of `sub f() is Foo(:a<b>)` arrived as a *named*
argument, so a `trait_mod:<is>(Routine:D, Foo, Pair $p)` candidate never
matched and the two-argument candidate ran instead (raku binds `$p` to
`:a("b")`). The declaration-time chunk deliberately mints a colonpair in the
named flavour (ADR-0021; `role R[:a(1)]` needs it), so the named-sub trait
dispatch now turns such a Pair back into the data flavour before passing it
positionally after the type object — which is what the anonymous-sub path
already did.

Pinned by `t/oo/trait/routine-qualified-type-trait.t`. `Path::Map`'s
`t/trait.rakutest` now parses; the distribution stays red for unrelated
runtime gaps in its own module.
