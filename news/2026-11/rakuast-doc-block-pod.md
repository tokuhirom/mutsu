# `EVAL` of a hand-built `Doc::Block` establishes `$=pod`

A `RakuAST::Doc::Block` statement in a tree given to `EVAL` ran nothing and
left `$=pod` empty (`$=pod.elems` was 0; rakudo 2026.09 answers 1). The block is
now written back as the Pod source it would have been read from and handed to
the builder `$=pod` is made with (#14880), so a hand-built delimited, `for` or
abbreviated block, nested blocks and paragraphs become the same `Pod::Block`
objects as when written in source. Blocks with configuration, a margin or table
rows are not written back yet and leave `$=pod` as it was.
