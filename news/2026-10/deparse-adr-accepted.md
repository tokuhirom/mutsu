# ADR-14845 (`.DEPARSE` runs rakudo's deparser) is Accepted

The maintainer accepted the decision and an amendment records the implementation as it is: a source-substring
trigger injects the vendored prelude once per process (not precompilation-cached), the only load-time textual
change is the class head, the vendored file is rakudo `main` at `9a94801c` rather than a release tag, and the
`.DEPARSE` parity ratchet is still to be built (#15792). Vendoring `RakuAST::Deparse::Highlight` is #15793.
