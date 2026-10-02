# A pointy block argument names a parameterized role `Q[Block]`

`my role Q[&f] {}; Q[-> $, $ { 1 }].^name` said `Q[Sub]`; Rakudo says `Q[Block]`. A curried
role names each argument after its type, and `what_type_name` classified every code value as
`Sub`. It now delegates to the same classification `.^name` uses, so blocks are `Block`, methods
`Method` and `WhateverCode` stays `WhateverCode`. Pinned by `t/types/role-parameterized-code-arg-name.t`.
