# `is built(:bind)` is recorded and encoded in the BUILDPLAN

The parser skipped the `(:bind)` argument of `has $.e is built(:bind)`, so the attribute was
treated as plain buildable and its BUILDPLAN task was `0`. The trait is now recorded
(`BuiltTrait::{No, Yes, Bind}`), and `.^BUILDPLAN` encodes it as Rakudo does: `1300` for a
bound object attribute's init, `1400` for its default; a native attribute keeps its ordinary
tasks (#14260). Binding the caller's container itself is tracked in #14559.
