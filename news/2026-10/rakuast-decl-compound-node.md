# A declaration's compound assignment is a typed statement node

`my $x *= 5`, `my $n foo= 6` and a private attribute's `has $!g //= 5` are now
`ParsedStmt::DeclCompound` (#14948): the declaration, the base infix and the
right-hand side. The lowering, `rakuast::decl_compound_assign::lower_parsed`,
expands the node into the declaration and then the compound assignment to it.
It is shared by:
- the parser, for every `my` compound declaration in both parses;
- a `.AST` parse of an attribute;
- `EVAL` of a hand-built tree.

The view renders the node as the `ApplyInfix` whose left operand is the
declaration, as rakudo 2026.09 does.

This retires the Stage 1 record the converter used to read the form back. The
following are deleted:
- `SourceForm::DeclarationCompoundAssign`;
- `ast::decl_compound_assign::{Source, parts, split_for_modifier,
  for_modifier_parts}`;
- the attribute expansion's `keep_source` flag;
- the converter's two record arms and `declaration_compound_assign_statement`,
  which rebuilt a postfix `for` the parser had hoisted.

A postfix `for` now hoists the declaration in the modifier lowering, the same
way for every declaration.

The `.AST` text and the results are unchanged.
`t/rakuast/rakuast-decl-compound-node.t` adds the word, user and attribute
forms to `t/rakuast/rakuast-declaration-compound-assign.t`.
