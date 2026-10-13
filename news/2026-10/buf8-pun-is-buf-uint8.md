# `buf8.^pun` (and the other native-sized buffer spellings) is the `Buf[uint8]` class

`nqp::create(buf8.^pun)` died with `Unknown role: buf8`, which stopped path-utils' first test file after 11 of 57
tests. `buf8`, `blob8`, `buf16`, ... pun to `Buf[uint8]`, `Blob[uint8]`, `Buf[uint16]`, ... like Rakudo. path-utils'
`t/01-basic.rakutest` now passes; `t/02-selective-importing.rakutest` still needs `UNIT::{"&name"}` to see a
`my constant &name` from inside a closure (#13272).
