# `also is Parent` applies at its position in the class body

Rakudo executes `also is Parent` **where it stands in the class body**, so
everything the body established before that line is visible to it. mutsu's
parser hoists the statement out of the body onto the declaration's header
`parents` vector, and `validate_class_parents` then resolved that list before
any of the body had run. Any parent the body itself brought into scope was
therefore invisible, and the declaration died as
`X::Inheritance::UnknownParent`.

Two shapes hit this, both minimised from real distributions:

```raku
class Kid {
    use Parentish;          # the parent arrives from a `use` inside the body
    also is Parentish;
}

class UC2 {
    class Inner { }         # the parent is a class the body itself declares
    also is Inner;
}
```

The first is every one of `Font::AFM`'s 14 `Font::Metrics::*` compunits (a
`blocked_load` in the ecosystem ledger); the second is `Intl::CLDR`'s five
`CLDR::*` format systems, all of the shape `unit class CLDR::X; ... class
Selector is Positional { ... } ... also is Selector;`. Note that the *header*
spelling of the second already worked -- `class Outer { class Inner { }; class
Sub is Inner { } }` resolves `Inner` to `Outer::Inner` correctly. Only `also
is`, applied to the enclosing class, failed.

## What changed

The parser is the only producer of these parents, so it now says so: the names
it lifts out of the body are recorded a second time in a new
`Stmt::ClassDecl::body_parents`, which rides alongside the existing
`does_parents`/`hidden_parents` vectors through `CompiledClassDeclPlan` and
`ClassDeclModifiers` into registration.

`validate_class_parents` consults that marker only once every other diagnosis
has had its say: a parent named by `also is` that would otherwise raise a plain
unknown-parent error is collected rather than thrown, so deferral can never
replace a more specific verdict (`X::InvalidType`,
`X::Inheritance::Unsupported`).

It does sit **ahead of** the custom `trait_mod:<is>` deferral, though, and that
ordering is the whole design point. Since
`news/2026-09/an-uppercase-is-trait-is-still-a-trait.md` that deferral claims
*every* unknown name as soon as any `trait_mod:<is>` is in scope -- which merely
importing `Test` arranges -- so running it first would swallow every `also is`
parent a body introduces, in exactly the files a regression test lives in. The
two stay compatible in both directions: a body parent the body turns out **not**
to introduce is handed back to the trait dispatch once the body has had its
chance, complete with the rollback snapshot that site needs, and raises
`X::Inheritance::UnknownParent` from there when no candidate claims it. `also is
Marked` therefore behaves exactly like `is Marked`, phantom-ancestor-free MRO
included.

The header phase then runs without the deferred name -- dropped
together with its position-aligned bracket-argument chunks, so the two stay in
lockstep -- and the new `registration_class_deferred_parents.rs` re-resolves it
once `run_class_body` returns, still before `finalize_class_registration`
computes the C3 MRO, which is the last point at which a parent can be added.

Re-resolution tries the class's **own package scope first**
(`UC2::Inner` before `Inner`), which is what the bare name means at that point
in the body and mirrors the scoping the header form already gets for a sibling.
A deferred name that turns out to be a role is composed rather than inherited,
merged into the header's own composition record instead of replacing it; one
that names the class itself is `X::Inheritance::SelfInherit`; and one that is
still unknown after the body raises exactly the error it always did, just
later.

`t/oo/class/also-is-body-position.t` pins all seventeen behaviours -- both
shapes in block and `unit class` form, the role and self-inherit cases, the
trait fall-through, and the still-unknown error -- and passes verbatim under
rakudo 2026.07. `registration_class_validate.rs` was already 543 lines before
this change, so its parent-validation half moved out to
`registration_class_parents.rs`.
