# RakuAST: `Block.literalize` is the `Map` of its statements' literal values

Remaining half of #16529. `{ 1, 2 }.AST.literalize` answered `RakuAST::Node::CannotLiteralize`;
rakudo literalizes a block (and a pointy block) as `Map.new(...)` of the literal values of its
statements, nested lists spliced in, and an odd count dies with `X::Hash::Store::OddNumber`.
