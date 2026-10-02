# Match positions count graphemes; methods can assign outer `my &k`

Two gaps found by working the MVC::Keayl distribution:

- `Match.from`, `.to` and `.pos` now report grapheme indices (the unit `Str.substr`
  and `Str.index` use) instead of raw codepoint offsets, so a subject containing
  `"\r\n"` no longer skews every position after it. Multipart body parsing in
  `t/controller/params.rakutest` relied on `$part.substr($m.to)`.
- A method body can assign through an enclosing `my &k` Callable container
  (file scope or type body, including `unit class`). The method compilers are
  scope-blind, so `&k = ...` was compiled as an assignment to a routine name and
  threw "Cannot modify an immutable value"; they now learn the enclosing `&`-lexicals.

Pinned by `t/regex/match-from-to-counts-graphemes.t` and
`t/oo/method-assigns-outer-callable-lexical.t`.
