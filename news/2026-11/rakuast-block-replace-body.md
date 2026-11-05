# RakuAST::Block.replace-body

`RakuAST::Block.replace-body($new-body)` swaps the `Blockoid` of the node in
place (every holder of the node sees the change), answers `Any`, and is
inherited by `PointyBlock` and the routines. A value that is not a
`RakuAST::Blockoid` raises `X::TypeCheck::Binding::Parameter`, as on rakudo
2026.09. `RakuAST::Block` joins `RakuAST::Name` in the owner list of the node
dispatch shape, so a one-argument `Block` row is reachable.
