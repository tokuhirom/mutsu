# RakuAST: the shape of a hyper compound assignment

`@a »~=» @b` is `ApplyInfix(left, MetaInfix::Assign(MetaInfix::Hyper(Infix "~")), right)` in
rakudo's `.AST`; mutsu rendered a `MetaInfix::Hyper` over `Infix "~="`. The conversion now wraps
the base operator the way rakudo does, `EVAL` of that tree lowers it back to the hyper operator,
and a hyper comparison that merely ends in `=` (`»<=»`) keeps its plain shape. The conversion
moved out of `view.rs` into `hyper_assign.rs` (lowering its frontend-debt ceiling to 6572 lines),
and the "is this an assignment meta-operator" test is now one function shared with the runtime
(#16381).
