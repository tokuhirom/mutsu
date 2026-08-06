# Angle-bracket word lists no longer warn about "leading 0" octal typos

`<0 10 021 1320 02431>` and similar `<...>` quote-word lists were emitting a
spurious `Potential difficulties: Leading 0 does not indicate octal in Raku`
warning for any word that looked like a leading-zero integer (`021`, `02431`),
even though the words are string-literal `q:w` content, not numeric-literal
syntax — raku emits no such warning here.

Root cause: `<...>` list parsing produces allomorphic values (`IntStr`,
`RatStr`, ...) by feeding each word through the same `integer()` parser used
for real numeric literals in expression position, and that parser
unconditionally records the leading-zero warning as a side effect. `<021>`
correctly becomes `IntStr` with `.Int == 21` (decimal, not octal) and
`.Str eq '021'` in both raku and mutsu — only the warning was wrong.

Fix: split `integer()` into a shared `integer_impl(input, warn: bool)`, keep
`integer()` (used at expression-parsing call sites) warning as before, and add
a new `integer_no_warn()` for the angle-word allomorph path
(`src/parser/primary/container/allomorph.rs`). Found via the real-dist
compatibility sweep (`String::Splice`'s test suite,
`todo/tickets/dist-test-suite-failures-batch.md`). Pinned by
`t/angle-word-leading-zero-no-warn.t`.
