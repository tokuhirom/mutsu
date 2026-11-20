# RakuAST origins of packages written as expressions

A class, role or grammar written in expression position (`my $c = class {
method a { 1 } }`, `class :: is Int { }`, `grammar { token TOP { a } }.parse`)
had no `.origin` on its body or anything in it: the expression parser recorded
no package spans and read the body without its statements' positions. It now
reads the body as the statement form does (`package_body_block`) and records
the spans, so the block, its statements, methods and tokens, the name (the
`::` of an anonymous `class ::`) and the traits are placed as rakudo 2026.11
places them (#15780).
