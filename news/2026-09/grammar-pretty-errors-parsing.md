# Grammar::PrettyErrors now reports wrapped grammar failures correctly

Grammar roles that override `parse` and install wrapped token regexes now run
through the grammar instance's constructor, so per-instance wrappers are
visible during parsing. Wrapped grammar tokens also keep their rule frames in
backtraces, including `hidden-from-backtrace` methods, and dynamic regex
methods added with `.^add_method` participate in the wrapper chain.

Rule-separated quantifiers with implicit rule whitespace retain their native
separator structure. This keeps multiline failures' `*HIGHWATER` position and
last-rule context accurate for `Grammar::PrettyErrors`.

Pinned by `t/grammar/grammar-rule-separated-quantifier-whitespace.t` and the
`Grammar::PrettyErrors` ecosystem test suite.
