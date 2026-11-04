# `.Str` of a Pod block is its text

`.Str` of a `RakuAST::Doc::Block`, `Doc::Paragraph`, `Doc::Markup` or
`Doc::LegacyRow` answered the node's `.gist` (the `RakuAST::Doc::Block.new(...)`
rendering). It is now the text of the Pod, as rakudo answers it (#14880): a
block is its paragraphs run together, a formatting code is put back with its
delimiters and metadata (`B<x>`, `X<a|b>`, `E<gt;lt>`), and a table row is put
back as written (cells two spaces apart, or between `|` dividers).
