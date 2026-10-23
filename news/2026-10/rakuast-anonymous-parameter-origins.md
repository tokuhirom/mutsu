# RakuAST anonymous parameters are spelled and placed as in Rakudo

An anonymous variable in a declaration signature (`my ($, $b) = 1, 2`) showed
up in `.AST` under the parser's internal name, `$__ANON_STATE#`, where Rakudo
has the bare sigil `$`. The conversion now spells `$`, `@` and `%` elements
with their sigil, and lowering a hand-built signature maps the sigil back, so
`EVAL` of the tree still declares nothing for them.

`visit-children` of the `ParameterTarget::Var` of an anonymous target (in a
declaration signature or a routine's, typed or not) now visits a
`VarDeclaration::Anonymous`, as Rakudo does, instead of a
`VarDeclaration::Simple`. Its range, and the target's, is the sigil: the
parser finds the bare sigil of an anonymous routine parameter in the
parameter's text, and the declaration signature, which used to be left
without a range when it held an anonymous element, is now placed too.
