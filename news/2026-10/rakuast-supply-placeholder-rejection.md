# RakuAST preserves placeholder scope errors in supply and start blocks

RakuAST conversion now preserves the source-level `supply` and `start` prefix
when placeholder scope checking has desugared a block. Lowering reaches the
same `X::Placeholder::Block` diagnostic as ordinary parsing, while valid supply
blocks continue to execute normally. Regression coverage pins both behaviors.
