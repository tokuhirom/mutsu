# ADR-15371: The BEGIN-time resolver (`$*R`) and compunit (`$*CU`) surface a module may call

- **Status**: Proposed
- **Date**: 2026-11-08
- **Deciders**: ecosystem inventory worker, for the maintainer's review
- **Issue**: [#15371](https://github.com/tokuhirom/mutsu/issues/15371) (CompUnit::Util,
  Archive::SimpleZip); same host object as [#13369](https://github.com/tokuhirom/mutsu/issues/13369),
  [#13370](https://github.com/tokuhirom/mutsu/issues/13370), [#15804](https://github.com/tokuhirom/mutsu/issues/15804)
- **Related**: [ADR-15762](15762-slangs-by-execution.md), [ADR-16888](16888-statementlist-override-by-the-slang-host.md),
  [ADR-10723](10723-rakuast-is-the-frontend-ir.md), [ADR-0096](0096-batteries-adoption-policy.md),
  [docs/security.md](../security.md)

## 1. Context

`CompUnit::Util` 0.7.0 (and Archive::SimpleZip, which `use`s it) is written against rakudo's
RakuAST frontend. At BEGIN time it reads `$*R` (`RakuAST::Resolver::Compile`) and `$*CU`, calls
`$*R.current-scope`, `merge-generated-lexical-declaration` and builds
`RakuAST::Declaration::Import` nodes. The first half of #15371 (`CompUnit::Handle.unit`,
`.export-sub`, `.globalish-package`) is done; `t/04-capture-import` now matches rakudo. The rest
(`t/02`, `03`, `05`, `06`, `08`, `09`, and `t/01` after test 12) dies with "... can only be
called at BEGIN time", because the module gates on `$*W || $*R` and mutsu offers no `$*R` outside
`sub EXPORT`.

Today `$*R` exists in exactly one place: `src/runtime/attach_target.rs` gives a module's
`sub EXPORT` a stub resolver with `find-attach-target` and `add-leave-phaser`. The same object
family is needed by the slang work (a slang action calls `$*R.current-scope` / `attach`), so
this is one host surface, not four.

Per the ecosystem-oracle decision (2026-10-21) the oracle is rakudo's RakuAST frontend, so
reproducing `$*W` (legacy) is out of scope.

## 2. Decision (proposed)

1. **One resolver object per compile unit**, `RakuAST::Resolver::Compile` by name, created by the
   Rust parser/lowering host and bound to `$*R` (and the unit's `RakuAST::CompUnit` to `$*CU`)
   for the dynamic extent of every BEGIN block, `sub EXPORT` and slang action of that unit. It
   is the object `attach_target.rs` already stubs; that file's handle becomes one of its
   answers, not a separate thing.
2. **Reads answer from the parser's scope state**: `current-scope` (the innermost
   `RakuAST::Block`/`CompUnit` handle), `find-attach-target`, `resolve-lexical` (a
   `RakuAST::Declaration` handle or `Nil`), `resolve-lexical-constant`, `get-package-variable`
   (only what a module actually calls; the list is grown from real upstream modules, never from
   rakudo's whole class).
3. **Writes are RakuAST nodes handed back, not mutation of parser internals**:
   `merge-generated-lexical-declaration`, `add-declaration`, `attach` take a `RakuAST::*` node
   built by user code (the nodes `rakuast::lower` already accepts, ADR-15334); the host
   validates the node kind and installs it in the scope being compiled, at the BEGIN point.
4. **Time and trust**: the surface exists only on the *run* path. Parse-only and analysis paths
   (`src/analysis/`, `crates/mutsu-lsp`, `--dump-*`) never run BEGIN-time user code, so they
   never see `$*R`; this is the existing rule of docs/security.md and is not relaxed. A node a
   module installs is checked like any parsed node (no new `unsafe`, no allocation sized by the
   module).
5. **No native substitute for CompUnit::Util** (ADR-0096): the vendored upstream module must run
   verbatim; the only mutsu code is the host object.

## 3. Alternatives rejected

- *Emulate `$*W`*: legacy frontend, no longer maintained upstream.
- *Native `re-export` / `steal-globalish` functions*: rung-3 substitute, banned by ADR-0096.
- *Expose the Rust `Scope` structs directly through NQP-style ops*: leaks parser internals and
  would make every parser refactor an ecosystem break.

## 4. Open questions

- Whether `$*R` should be created lazily (only when a BEGIN block or EXPORT first reads it);
  recommended, since most units never do.
- How far the first slice goes: recommended order is `current-scope` +
  `merge-generated-lexical-declaration` for `re-export` (t/02, 05), then `steal-globalish`
  (t/03), then `set`/`get`/`push-multi` (t/06, 08, 09).

## 5. Slices and size

Effort L, risk medium. Lane `rakuast` (shares the host object with ADR-15762). Dependencies:
ADR-15762 slice 1 (merged) for the activation point; independent of 13369/13370.
