# RakuAST origins of method literals, sigilless assignments and mixins

`.AST` now places, as rakudo 2026.11 does:

- the body of a method literal (`method { 1 }`). Its `Blockoid`, statements
  and expressions are placed as a sub literal's are;
- the braces of a sub or method literal that come after a signature
  (`sub ($a) { $a }`);
- a variable that ends a statement list when the parser left it unplaced,
  such as a parameter read again in the body, which the conversion cannot
  tell apart from the parameter;
- a sigilless term assigned to (`foo = 7`), with its `Name`;
- a dotted subscript interpolated in a string (`"@x.[1]"`, `"%h.<a>"`);
- the `but` / `does` operator (`Mixin`) of a mixin application (#15780).
