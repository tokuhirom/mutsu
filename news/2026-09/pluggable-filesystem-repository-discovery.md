# Pluggable can discover plugins from a `use lib` directory

Running Pluggable 0.6 from the ecosystem ledger exposed two missing pieces of
the `CompUnit::Repository::FileSystem` contract. Mutsu now represents plain
`use lib` paths in `$*REPO.repo-chain` and exposes recursive synthetic
distribution metadata for source repositories, so Pluggable's plugin
discovery and process tests reach parity with Rakudo.

The distribution moved from 1/4 to 3/4 parity files (1/7 to 6/7 assertions).
Its syntax-error case remains partial because an undeclared bareword in a
required module is still accepted; that general interpreter gap is tracked in
[#8986](https://github.com/tokuhirom/mutsu/issues/8986).
