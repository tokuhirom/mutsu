# RakuAST: the empty name edge (`Name::Part::Empty` / `EmptyEdge`)

A `RakuAST::Name` can now begin or end with an empty edge. It shows up in
three forms:

- the leading `::` of `::Foo` and `::($x)`;
- the trailing `::` of a stash lookup like `Foo::` or `MY::`;
- both edges at once, in a bare `::`.

Before this change mutsu had no model for it. `Q|Foo::|.AST` died with
"does not yet support this construct: PseudoStash", and `::("x")` rendered
without its leading `Name::Part::Empty.new`.

The shapes below were measured on Rakudo 2026.09, and mutsu now matches them:

- The leading edge is an instance (`RakuAST::Name::Part::Empty.new`). The
  trailing edge is the bare `RakuAST::Name::Part::Empty` type object, and
  `.parts` hands that type object back unchanged.
- A stash lookup is a `Term::Name` when its package resolves at parse time.
  That covers pseudo-packages, builtin types, types the unit declares and the
  stub `A` of `class A::B`. Otherwise it is an argument-less `Call::Name`.
  Either form lowers back to the stash through `EVAL`. Recording the stub
  package also makes a bare `A` after `class A::B { }` render as a
  `Type::Simple`, which is what Rakudo does.
- `RakuAST::Name.new(*@parts)`, `RakuAST::Term::Name.new` and
  `RakuAST::Call::Name.new(name => ..., args => ...)` can now be constructed.
  `Name` picks the spelling Rakudo prints:
  - `from-identifier` for one identifier part;
  - `from-identifier-parts("A","B")` for several (there is no space after the
    comma, which mutsu used to get wrong);
  - the general `Name.new(...)` form otherwise.
- No `Name::Part` value matches `RakuAST::Node` or `RakuAST::Name` any more,
  which is what Rakudo reports. Before this change only `Part::Expression` was
  excluded.

rakudo/rakudo#6771 renames `Name::Part::Empty` to `Name::Part::EmptyEdge`, so
the new spelling is accepted as well. It builds, lowers and renders under its
own name, while `.AST` keeps emitting `Empty` the way Rakudo 2026.09 does.

The regression test is `t/rakuast/rakuast-name-part-empty.t`; the GitHub issue
is #10646.
