# RakuAST regex arguments retain indirect callable calls

Constructed RakuAST regexes now lower lexical callable arguments such as
`&decorate($value)` back through the existing regex matcher, preserving
match-time callable lookup and evaluation.
