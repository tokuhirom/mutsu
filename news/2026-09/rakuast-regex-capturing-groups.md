# Regex capturing groups retain their RakuAST and execution shape

Parser-created and constructed regexes now retain ordinary positional capture
groups in the shared `RegexTree`. RakuAST exposes them as
`RakuAST::Regex::CapturingGroup`, while execution lowers them to the existing
capture-aware matcher without changing capture numbering or quantified
sub-Match values.
