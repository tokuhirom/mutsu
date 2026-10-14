# A backreference under `:m` compares exactly, marks included

Under `:ignoremark`, rakudo compares the pattern's own atoms by base but a
backreference (`$0`, `$<k>`) exactly, marks included, just as `:i` does not
fold one. mutsu runs a `:m` pattern over the mark-stripped subject, so the
captured text had lost its marks and `"a\x[301]a" ~~ /:m (a) $0/` matched.

A backreference over the stripped view now compares the two spans in the
original subject, so it fails there as in rakudo, while `"áá"` still matches.
This covers a whole-pattern `:m`, a scoped `[:m …]` group and a `:m` grammar
token (#13201).
