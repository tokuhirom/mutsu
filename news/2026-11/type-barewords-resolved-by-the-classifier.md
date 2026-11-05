# Type words are resolved by the classifier's answer

The native type words (`uint8`, `int32`, ...) written as barewords were the next
class of words the executing finalization converted to RakuAST and lowered back.
When the classifier (`bareword::view_node`) resolves a plain identifier to
`Type::Simple`, the lowering of the node hands the word back (`Nil` is its own
literal), so `bareword::type_word_expr` returns the word without building the node
tree (ADR-15760 slice 2). Like the term shortcut it applies only to an unspelled
word the finalization does not already carry as a type name. `.AST` and
`EVAL($ast)` are unchanged.
