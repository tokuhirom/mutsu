# RakuAST: WhateverCode `where` in a `:(...)` literal survives the round trip

A `where *.foo` or `where * == 2` clause in a signature literal stopped matching after `.AST` -> `EVAL`, because the `FakeSignature` lowering builds the `Signature` value directly and the program-wide WhateverCode pass never reached the parameters inside it. `lower_fake_signature` now runs that pass over the lowered parameters itself (#12293).
