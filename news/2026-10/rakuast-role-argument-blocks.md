# RakuAST round trip: code-valued role and assertion arguments

A parameterised role applied with a code-valued argument (`does R[{ 42 }]`,
`does R[{ $^n * 2 }]`, `does R[* cmp * == More]`) and a regex assertion given
an empty-signature pointy block (`<e(-> { ... })>`) now lower back from
RakuAST: the argument source the role and regex machinery re-read is spelled
from the lowered block, placeholder block, WhateverCode or pointy block.
The round-trip frontend ratchet grows by six files.
