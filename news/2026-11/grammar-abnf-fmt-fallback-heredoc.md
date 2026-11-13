# Grammar::ABNF: nested `List.fmt`, `FALLBACK` on Cool, heredocs in regex code

Grammar::BNF's ABNF suite (`t/04-abnf.rakutest`) went from 11 to 25 of its
29 tests. The rest need the slang package declarator (ADR-0091). Three
differences from rakudo were fixed on the way:

- `List.fmt` formats an element that is itself a `List` (an `Array` or a
  `Slip`, itemized or not) with the same format and separator. Another
  `Iterable` element (a `Seq`, a `Range` or an associative) uses the format
  and its own default separator. The one exception is a bare `%s`, which
  reads the element's `.Str`. `("C".ords,).fmt(Q[\x%x])` is now `\x43`, not
  `\x0`.
- A `Cool`-only method name such as `.comb` or `.uc` no longer goes to
  `FALLBACK` when the invocant's class is itself `Cool`. That covers a `Match`
  of a grammar that declares `FALLBACK`, and `class C is Cool`.
- A heredoc started inside a regex code block (`token { [ { die Q:to<ERR> }
  ]**1..* ⏎ … ⏎ ERR ⏎ x }`) takes its body from the lines after the block's
  line. Before, those lines were read as regex atoms, and the RakuAST frontend
  refused the token.
