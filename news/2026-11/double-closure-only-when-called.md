# "Malformed double closure" only for a block that is called on the spot

mutsu rejected every `{ ... }` whose only statement is a `*`-curry (`my $b = { * + 1 }`).
Rakudo's RakuAST frontend, the oracle, accepts these (the block returns the WhateverCode, as does
`do { * + 1 }`) and rejects only a call applied straight to the literal: `{ * + 1 }()`,
`{ *.abs }.()`, `({ *.abs })(1)`. This was the second-largest Qwiratry failure class
(`{ $_ ⪪ * ⪪ <people> }` with a user infix).
