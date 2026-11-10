# `item` of a code object stays callable

`my $c = item({ 42 }); $c()` died with `No such method 'CALL-ME' for invocant of type 'Block'`: `item` wrapped the
code object in a `Scalar` container that a call did not see through. A code object is one item already, so `item`
answers it unchanged. `RakuAST::Block.meta-object` is built as an `item(...)` call, so the closure Qwiratry's
`compile-block-expr` compiles for a mold's `when` query was not callable and `item mold has extracted when-query`
failed (#17243, `t/integration/mold-when-query.rakutest`).
