# RakuAST renders a string literal return value (`--> "foo"`)

A signature pinning its return value to a plain string (`sub f(--> "foo")`, `-> --> "done" { }`, `--> ''`) was refused as an unknown type. The converter now renders rakudo's `returns => QuotedString`, lowering spells it back (sub and pointy-block signatures alike), and strings that need interpolation or escapes stay refused. Three `t/` files join the round-trip ratchet.
