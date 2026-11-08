# ADR-15762 and ADR-15371 accepted: slangs run on a host, with a BEGIN-time resolver

Maintainer decision (2026-11-08). ADR-15762 ("Slangs by execution") is Accepted: a slang's new
productions are hooks into the Rust parser, and its user tokens and actions execute on the slang
host, replacing the recognised-override map as the only mechanism. ADR-15371 is Accepted on top of
it: the same host exposes one `RakuAST::Resolver::Compile` object as `$*R` (and the unit as `$*CU`)
to BEGIN blocks, `sub EXPORT` and slang actions, grown from what real upstream modules call. The
first slice is `current-scope` plus `merge-generated-lexical-declaration` for CompUnit::Util's
`re-export` (`t/02`, `t/05`). Parse-only paths (LSP, `--dump-*`) still never run user code.
