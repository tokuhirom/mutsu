# RakuAST PointyBlock constructors accept omitted signatures and bodies

PointyBlock.new now accepts no arguments, a body alone, or a signature alone.
The default body is an empty Blockoid and the existing signature accessor
supplies a defined empty Signature when the constructor omits one. Rendering
omits both absent and explicitly empty signatures, matching Rakudo.

Evaluation retains an explicit zero-argument callable, while supplied
signatures continue to enforce parameter types, arity, and return constraints.
