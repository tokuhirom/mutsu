# Regex `<{ }>`, `** {n}` and `:my` compile their code once

A regex's `{ … }` blocks and `<?{ … }>` assertions have long reused one compiled
chunk per fragment, keyed by the regex-code parse cache's id (ADR-0009). Three
other constructs that embed code did not
([#10121](https://github.com/tokuhirom/mutsu/issues/10121)), and each recompiled its
fragment from AST on every match attempt.

`<{ … }>` interpolation and the `** { … }` count simply called the uncached
`eval_block_value`. They now go through the parse-cache id, or through the parse
site for a body the parser already produced. The same change covers the other
sites of that shape: subrule argument expressions, the `CodeInterp` atom, and a
grammar rule's `:my $*x` re-derivation.

A leading `:my $x = …` was more interesting. It already used the cached path.
But the match snapshotted the grammar token table and restored it on the way
out, in case a `:my token` had been declared. Restoring takes a registry write
guard and bumps `TOKEN_DEFS_GEN`. The regex-code parse cache is keyed on the
registry write generation, so it handed out a fresh id on every match and the
compile cache could never hit. Every generation-keyed regex memo (the parse
cache, proto variant keys, the call-graph tables) was thrown away per match as
well. The snapshot is now taken only when a declarator actually declares a
token.

`MUTSU_VM_STATS`'s `carrier-compile:` line gained an `uncached=` field, which
counts compiles that had no cache key at all. That is how the fix is pinned:
`tests/regex_embedded_code_compiled_once.rs` asserts that `misses + uncached`
is the same at 10 and at 60 iterations for each of the three shapes.

Two findings are filed separately: token *declaration bodies* still compile per
evaluation ([#10266](https://github.com/tokuhirom/mutsu/issues/10266)), and a
`:my token` is not visible after its regex the way it is in rakudo
([#10267](https://github.com/tokuhirom/mutsu/issues/10267)).
