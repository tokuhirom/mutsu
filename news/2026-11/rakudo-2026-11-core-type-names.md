# CORE type names regenerated on Rakudo 2026.11

`src/core_type_names.txt`, the list of type names mutsu treats as declared by
the CORE setting, was the Rakudo 2026.09 snapshot though the pinned oracle
is 2026.11. It is now regenerated on 2026.11 (#19274), and each change was
checked against `raku`:

- The shaped-array rework's `Array::Jagged`, `Array::JaggedRow`,
  `Array::JaggedRows`, `Array::JaggedSlice`, `Array::JaggedSlots`,
  `Array::ShapedView` and `Hash::Shaped` now resolve to their type objects
  instead of dying with "Could not find symbol". Their behaviour is tracked
  separately: jagged shapes and row views in #19282, tuple-keyed hashes in
  #19283.
- `X::Syntax::Heredoc::AmbiguousName`, `X::Syntax::Heredoc::HiddenName` and
  `X::Whatever::ShortCircuit` resolve with rakudo's MRO and roles
  (`HiddenName` isa `X::Undeclared`; `ShortCircuit` is not an `Exception`),
  their constructor attributes, and rakudo's `.message` texts. Throwing the
  heredoc checks is #19284. The class catalogue now supports an `X::` row
  whose parent is `Any`, so `ShortCircuit` and the older
  `X::WhateverCode::SmartMatch::LHS` can be constructed and no longer
  smartmatch `Exception`, as on rakudo.
- The new RakuAST nodes (`MetaInfix::CrossZip`, `Name::Part::EmptyEdge`,
  `StatementModifier::IfUnless`, `Var::PackageHeaderLookup`,
  `VarDeclaration::Implicit::HeaderValue`, `WhereThunk`) were already known
  to mutsu's RakuAST model and are now listed.
- `RakuAST::Expression::QAST`, removed in 2026.11, no longer resolves.

`scripts/gen-core-type-names.raku` also learnt to walk a package whose
`WHO` is a raw NQP hash rather than a `Stash`. On 2026.11 `RakuAST::IMPL`
is one, so the plain walk dropped `RakuAST::IMPL::QASTContext` though rakudo
still resolves it, and it had always missed classes such as
`RakuAST::DottyInfix::Call` and `RakuAST::MetaPrefix::Hyper`. Only Raku
types are taken from such a hash; NQP bootstrap classes like
`ContainerDescriptor::Untyped` are not setting types.
