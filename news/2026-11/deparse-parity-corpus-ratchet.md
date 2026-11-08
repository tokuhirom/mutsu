# `.DEPARSE` parity corpus ratchet

`scripts/ast-text-corpus.sh` now has a `CORPUS=deparse` mode (#15792, ADR-14845 §2.5): the
`.DEPARSE` text of every top-level statement of a pinned sample of roast files, hashed from
rakudo 2026.09 in `ci/deparse-corpus/rakudo.tsv`, is compared with mutsu's. The 1532
statements that match today (15.3% of 10031 in 86 files) are listed in
`ci/deparse-corpus/identical.tsv`, which only grows; CI's `test-suites` job runs
`CORPUS=deparse scripts/ast-text-corpus.sh check` next to the `.AST` text ratchet.
Since the deparser is vendored rakudo code, a mismatch is a difference in the tree it reads
or in a mutsu primitive it uses.
