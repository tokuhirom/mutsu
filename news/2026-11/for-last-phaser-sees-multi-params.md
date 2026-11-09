# `LAST` in a `for` with several parameters sees the final iteration's values

A `LAST` phaser is lowered to run after the loop, where the body's lexicals are gone. Only
`$_` and a single `-> $x` parameter survived, so `for @a.kv -> $i, $e { LAST { say $i } }`
read `Nil`. Each iteration now copies the multi-parameter loop variables into temps and the
`LAST` body re-declares them (JSON::RPC's batch-response matching hung off `LAST { ... if
$position != $responses.elems - 1 }`; its `t/00-client.rakutest` now matches raku, 39 tests).
