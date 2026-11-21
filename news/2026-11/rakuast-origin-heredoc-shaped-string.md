# RakuAST: a heredoc keeps its own origins beside a string of its shape

In `is qq:to/E/, "@l[] @l[0]\n", ...` the string argument has the same shape
as the heredoc's body, and the term positions the parser noted for the one
went to the other: the heredoc's body was placed over the string, and its
own lines were left unplaced (#15780). A heredoc body whose range falls
outside the lines it was written on now drops those borrowed origins and is
placed again from its own lines (`heredoc_body_origins.rs`).
