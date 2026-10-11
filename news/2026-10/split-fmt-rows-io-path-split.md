# `IO::Path.split` splits the file, and `split`/`fmt` are method-table rows

`"file.txt".IO.split(",")` split the path string and answered `("file.txt",)`. Like Rakudo, it now
reads the file and splits its contents, with `:enc` and the other open adverbs applied to the
file and every other argument (`:skip-empty`, a limit, a regex) to the split.

`split` on `Str`, `Cool` and `IO::Path`, and `fmt` on `Cool`, are method-table rows sharing one
routine with the dispatch cascades (ADR-11276 §9.76). That completes the `text` group of the
migration inventory. The string `split` fast path also reads a `*`, `Inf` or `NaN` limit the way
the interpreter does.
