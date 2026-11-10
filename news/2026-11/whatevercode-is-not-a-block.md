# `WhateverCode` no longer matches `Block`

`* + 1 ~~ Block` and a `Block:D` parameter accepting a WhateverCode were true in
mutsu; in rakudo a WhateverCode is a `Code` only. ParaSeq's `grep(Block:D)` /
`grep(Callable:D)` multis therefore picked the Block candidate for `* %% 2` and
died calling `callable_for_phaser`.
