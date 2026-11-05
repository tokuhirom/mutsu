# The `.AST` text corpus is a CI ratchet

`scripts/ast-text-corpus.sh` measured how many top-level statements of a sample of files render the same
`.AST` text as rakudo, but only on demand and only with `raku` installed. The runner has no `raku`, so the
rakudo side is now committed as hashes (`ci/ast-text-corpus/rakudo.tsv`: 10407 statements of 85 sampled roast
files, rakudo 2026.09) and `ci/ast-text-corpus/identical.tsv` lists the 9124 statements (87.7%) whose mutsu text
matched. The new `RakuAST .AST text ratchet` step of `test-suites` fails when a listed statement stops
matching; the list only grows (`scripts/ast-text-corpus.sh update`), and a roast file changed by a re-vendor is
skipped until `golden` is run again. This is the replacement gate the `MUTSU_RAKUAST=convert` coverage mode
needs before it can be folded into `.AST` (RakuAST C1, #15759).
