# `Doc::Paragraph.raku` breaks a single atom over lines

`RakuAST::Doc::Paragraph.new("q").raku` printed `RakuAST::Doc::Paragraph.new("q")`
on one line. That is the generic layout for a node whose fields are all
positional leaves. Rakudo puts a paragraph's atoms on their own lines whenever
it has any, standalone or nested in a `Doc::Block`'s `paragraphs`. mutsu now
does the same, and an empty paragraph still prints `.new()` (#19039).
