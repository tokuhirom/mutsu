# Formatter.AST returns an executable RakuAST node

`Formatter.AST` now returns a genuine `RakuAST::PointyBlock` instead of `Nil`. The model accepts a
slurpy argument list and calls `sprintf` with the captured format string, so it is both
introspectable and executable through `EVAL`.

The new regression test checks the node hierarchy, signature shape, and an evaluated formatting
round trip. This closes the last failure in `roast/S32-str/format.t`, taking the file from 48/49 to
49/49 and adding it to the roast whitelist.
