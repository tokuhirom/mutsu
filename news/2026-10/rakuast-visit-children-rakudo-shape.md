# RakuAST `visit-children` walks Rakudo's child shape

A tool that walks a tree with `visit-children` (Code::Coverable does) saw a
different set of children on mutsu than on Rakudo in three places:

- `ApplyInfix` visits its `Infix`, then an `ArgList` holding both operands
  (mutsu used to visit `left`, `Infix`, `right`);
- `ParameterTarget::Var` visits the `VarDeclaration::Simple` it declares, which
  carries an explicit parameter type;
- `VarDeclaration::Simple` visits no `desigilname` `Name`.

The shape is built in `children()` only, so the constructor form, `.raku` and the
lowering are unchanged.
