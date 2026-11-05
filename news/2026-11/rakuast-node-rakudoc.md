# RakuAST: `Node.rakudoc` and `Doc::Declarator`

`RakuAST::Node.rakudoc` answers the documentation nodes of a tree as a `Seq`, as rakudo does: the
descendants in pre-order (the node itself is not visited), a `Doc::Block` as it is and a
declaration carrying `#|` / `#=` docs as the new `RakuAST::Doc::Declarator` (`WHEREFORE`,
`leading`, `trailing`; constructor, accessors, `.raku`). Closes the `rakudoc` item of #14916.
