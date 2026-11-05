# Hyper function operators are carried through finalization

`@a >>[&f]<< @b` (`Expr::HyperFuncOp` over a plain `&f`) is kept by `finalize_unit` as the parser
built it instead of being converted to a `MetaInfix::Hyper` node around a function infix and lowered
back (ADR-15760 slice 2). A qualified or operator function (`&infix:<*>`) still goes through the
conversion. `t/rakuast/rakuast-hyper-func-op-finalize.t` pins the semantics.
