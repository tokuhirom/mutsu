# The divergence page now includes decisions found in closed issues and PRs

`docs/rakudo-divergences.md` started with the divergences that ADRs, `docs/` and
`news/` already recorded. A sweep through closed issues, issue comments and PR
bodies found more decisions to keep behaviour that differs from rakudo. The page
now lists them too:

- `CALLERS::` follows 6.e (#17846).
- A trailing delete shortens an array, as roast requires (#10985).
- A grammar token wrapped after a parse is honoured (#9189).
- A custom `ClassHOW` keeps role state per instance (#16239).
- An imported proto merges with the program's (#18587).
- `.^methods` hides rakudo-internal hooks (#13494).
- Rakudo-specific internals are not provided: `MoarVM::`, the hidden core
  `from-json` and Rakudo-Type-Introspection.
- RakuAST trees omit compile-time-only nodes (#14820).
- `zprintf` is a mutsu extension.
- Around twenty smaller quirks are listed in the C8 table, each with the issue or
  PR that decided it.
