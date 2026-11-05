# Triple-delimited formatting codes in Pod paragraphs

`I<<< q >>>`, `C<<<a b>>> x` and the like were left out of a block's tree. They
are now a `RakuAST::Doc::Markup` with `opener => "<<<"` and `closer => ">>>"`
(#14880), and the content is one raw string, as in rakudo 2026.09: blanks are
kept and a nested `<y>` is text, not a code. Codes with metadata, entities and
four or more brackets stay out of the tree.
