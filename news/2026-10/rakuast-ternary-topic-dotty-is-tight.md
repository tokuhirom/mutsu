# A bare `.=meth` in a ternary branch is a tight operator in a `.AST` parse too

`1 ?? .=uc !! 2` is legal because `.=` binds at method-postfix precedence. A `.AST` parse
(and so `MUTSU_RAKUAST=1`) keeps the bare topic form as a `CompoundAssign` node with the
`TopicDotty` meta, which `is_dotty_assign` did not recognise, so the ternary rejected it as a
loose assignment. `t/control/ternary-bare-dotty-assign.t`, the one `t/` file that passed
normally and failed under `MUTSU_RAKUAST=1`, is now in `ci/rakuast-frontend-passing.txt`
(8,712 of 8,712 `t/` files).
