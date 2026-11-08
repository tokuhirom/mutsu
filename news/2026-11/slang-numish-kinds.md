# A slang's `numish` override sees `NaN`, `Inf`, `∞`, unums and `<1/2>`, with rakudo's captures

The `numish` action override now runs for `NaN`, `Inf`, `∞`, `<1/2>` and `<1+2i>` as well, and the
`Match` it gets carries `integer`, `decimal-number` or `radix-number` only for an ASCII integer,
decimal or radix literal (a unum such as `Ⅼ` or `⅔` has none, as on rakudo). The real
overload::constant `t/01-basic.rakutest` now gives the same 13 `ok` as `raku` (#15804).
