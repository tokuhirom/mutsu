# A bare `is trait` on a class inside `unit module` reaches its trait

```raku
unit module M;
multi trait_mod:<is>(Mu:U $t, :$foo!) { say "hi {$t.^name}" }
class X is foo { }
```

This died with "'M::X' cannot inherit from 'M::foo' because it is unknown"
(#11349). Inside a `unit` package the compiler package-qualifies each parent
name on the guess that it is a type of the package. A name that turns out to
be a trait was then dispatched as `:M::foo`, which no candidate accepts.

For each parent it rewrites, the compiler now records the source spelling as a
`__parent_spelling` declaration trait, and a deferred `is foo` is dispatched
as `:foo`. A hoisted class shell carries the header's parents but not its
traits, and it no longer dispatches the deferred trait as well, so the trait
runs once.

The role side runs its trait twice in some programs; that is #11605.
