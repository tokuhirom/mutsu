# `RakuAST::Node.set-origin`

`$node.set-origin($origin)` gives the node the `RakuAST::Origin` (the very
object) and answers it, as rakudo does; it accepts only an origin. A hand-built
node then reads that origin from `.origin`. (#14916)
