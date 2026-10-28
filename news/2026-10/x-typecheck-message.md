# `X::TypeCheck.message` is built from `operation`, `got` and `expected`

A hand-built `X::TypeCheck.new(operation => ..., got => ..., expected => ...)`
had an empty `.message` in mutsu. It now reads like Rakudo's: `Type check
failed in fetching; expected Int but got Str ("x")`, with a value of the
expected type shown alone and a long value cut to 23 characters. Hash2Class
matches on this text, and its `t/01-basic.rakutest` now passes 42/42.
