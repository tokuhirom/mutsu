# dd of a RakuAST node prints its source

`dd $node` now prints the DEPARSE of a RakuAST node (`dd
RakuAST::Name.from-identifier-parts('IO','Path')` prints `IO::Path`), as
rakudo 2026.11 does and as the RakuAST type docs show, instead of its
constructor form. A node inside a container still prints as `.raku`. The
deparser call is now one `Interpreter::deparse_node` shared by `.DEPARSE` and
`dd` (#18980).
