# RakuAST: modified one-line statements, statement-level `@`, indirect name calls

Three more `.origin` and shape gaps closed (#15780):

- a statement with a modifier in a one-line anonymous body
  (`sub ($y) { say $y if $y }`) now runs through its modifier; it took the
  range of its call alone (`anon_body_origins::bare_statement`,
  `listop_origins::unmarked_statement`);
- a bare `@` assigned to as a statement (`@ = 1, 2;`) is a
  `VarDeclaration::Anonymous`, as `%` already was; it rendered as a
  `Var::Lexical` of the parser's internal name (`anon_state::is_array`);
- an indirect name called with no arguments (`GLOBAL::(q[$x])`) has a `Name`
  spanning the whole call and an empty `ArgList` at its end
  (`indirect_name_origins::call`).
