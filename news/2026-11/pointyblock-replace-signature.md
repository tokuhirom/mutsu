# `PointyBlock.replace-signature` and `Block.set-implicit-topic`

`RakuAST::PointyBlock` accepts `replace-signature` (it died with "only available on
RakuAST::Routine"), and a block has `set-implicit-topic($on, :required, :exception)`.
Qwiratry's `BodyRewrite.give-body-topic` needs both to give an `aggreduce`
body a `$_` parameter. Covered by `t/rakuast/rakuast-pointyblock-replace-signature.t`.
