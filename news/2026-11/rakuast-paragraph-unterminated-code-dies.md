# `Paragraph.from-string` dies on an unterminated formatting code

`RakuAST::Doc::Paragraph.from-string("a B<b c\n")` returned the plain string.
On rakudo it dies with `RakuDoc markup code B missing endtag '>'.` (#19040).

The Pod formatting-code reader now says why it declined a paragraph. A code
whose closer never comes is reported with its letter and closer: the
innermost such code, for `<`, `<<`, `<<<` and `«` delimiters and for codes
with metadata (`L<x|y`). `from-string` and `Doc::Block.from-paragraphs` raise
rakudo's error for it.

The reader itself moved from `pod_blocks.rs` into `pod_blocks/atoms.rs`.
