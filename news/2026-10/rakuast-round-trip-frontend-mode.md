# A round-trip frontend mode measures how far RakuAST is from being the frontend

ADR-10723 makes RakuAST mutsu's frontend IR: the parser will emit a RakuAST tree
and `lower` will be the only place that desugars it. Rakudo ran its own migration
behind `RAKUDO_RAKUAST=1` and published pass counts until they reached parity;
mutsu now has the same instrument.

With `MUTSU_RAKUAST=1`, the main program and every `EVAL` string are converted to
their RakuAST tree and lowered back before they are compiled. That is exactly the
tree the RakuAST frontend will hand the compiler, so a test file that passes in the
mode is one the new frontend can already run. A construct the converter or the
lowerer refuses is a compile error, never a fallback to the parser's own tree,
because a fallback would count a file that does not round-trip.
`MUTSU_RAKUAST=all` also round-trips every `use`d module and `require`d file; it is
a separate level because the bundled `Test.rakumod` does not round-trip yet, which
would fail every test file at its first line.

The first count is **984 of 5696** `t/` files. They are listed in
`ci/rakuast-frontend-passing.txt`, and CI's `test-suites` job runs them in the mode
(`scripts/rakuast-frontend.sh check`): a listed file that stops passing is a
regression in the RakuAST layer. `scripts/rakuast-frontend.sh update` grows the
list after a fix. The roadmap and the count over time are in #7564.
