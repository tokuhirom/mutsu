# RakuAST: an array inside a hash value literalizes by the array rule

`{a => [1]}.AST.literalize` held `(1,)` where rakudo holds `1`: a hash composer was evaluated as a
whole, so its values never went through `literalize`'s `[ EXPR ]` rule. A composer of fat arrows
now literalizes entry by entry (rest of #16529).
