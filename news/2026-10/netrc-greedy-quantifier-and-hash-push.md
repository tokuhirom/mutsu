# Explicit greedy quantifier marker, Hash.push through containers, List.Slip Nil

Making `Net::NetRC`'s own suite pass exposed three general gaps:

- Regex quantifiers accept the explicit greedy marker `*!`, `+!`, `?!` (the counterpart of the frugal
  `*?`); it was rejected as an unrecognized metacharacter.
- `Hash.push`/`append` look through the container of a Pair read from a `$` variable inside a list,
  and a duplicate-key push wraps an immutable List value instead of splicing it into the stack.
- `(Nil,).Slip` keeps `Nil` for an immutable List (only real Arrays default their holes to `Any`).

Net::NetRC's `t/01-basic.rakutest` and `t/02-functionality.rakutest` now both pass.
