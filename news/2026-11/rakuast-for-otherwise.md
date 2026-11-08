# RakuAST: `Statement::For` keeps `otherwise` and answers `replace-otherwise`

`RakuAST::Statement::For.new(:otherwise($block))` used to accept the argument
and throw it away, and the node had neither an `otherwise` accessor nor
`replace-otherwise`. Both now behave as on rakudo 2026.09: the block is hidden
state that `.raku` and the tree walks never show, `.otherwise` answers the very
object given (the `RakuAST::Block` type object when none was), and
`replace-otherwise` accepts a `Block` or `PointyBlock`, refuses anything else
with `X::TypeCheck::Binding::Parameter`, and answers `Nil`.

Running the block (`for @empty { } otherwise { ... }`) is a separate
compiler/VM change tracked with the Slang::Otherwise work on #13370.
