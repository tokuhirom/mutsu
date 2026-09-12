# RakuAST exposes the static regex tree

Static regex literals and the basic `regex` / `token` / `rule` declaration
cluster now retain a source-level `RegexTree`. `.AST` maps that tree to
`QuotedRegex`, the structural `Regex::*` nodes, and the matching declaration
nodes, including `Grammar` and boolean `m:i` / `m:g` adverbs. `EVAL` lowers the
supported tree back through the existing compiler and VM matcher.

Dynamic assertions, interpolation, captures, subrules, and adverbs with
runtime arguments remain explicit RakuAST boundaries until their source
expressions have safe tree and lowering support.
