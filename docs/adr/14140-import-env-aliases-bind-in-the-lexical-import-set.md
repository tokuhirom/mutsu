# ADR-14140: Import env aliases and the block-level module-merge grant bind in the lexical import set

- **Status**: Proposed (2026-10-30). Completes the "env" half of
  [ADR-8751](8751-imports-bind-lexically-in-the-importing-block.md) stage 1.
- **Date**: 2026-10-30
- **Addresses**: [GitHub issue #14140](https://github.com/tokuhirom/mutsu/issues/14140)
- **Related**: [ADR-12529](12529-frames-resolve-names-through-the-static-link.md),
  [ADR-14843](14843-fixed-frame-names-live-in-the-frame-record.md),
  [ADR-14792](14792-fixed-frame-names-are-env-first-with-record-fallback.md)

## 1. Context

ADR-8751 stage 1 exits when `imported_env_aliases`, the `ImportScope` registry
snapshot and `lexical_use_modules` are gone. The routine-table half is gone
(#15604: a registry-path import undoes its own logged writes). What remains is
the env half: `ImportScopeSnapshot::{imported_env_keys, shadowed_env_values}`,
`record_import_env_key` and `restore_import_env_keys`.

A census of every `record_import_env_key` call over `t/modules` and
`roast/S11-modules` (2026-10-30) shows what still binds through the env while an
import scope is open:

1. **Types and terms the parse-time scan cannot see**: a module that exports
   `\long`, `Tagged`, an enum, or a class through `is export`, for which
   `exported_types::record` stored `None` or no types.
2. **A `sub EXPORT` hook's returned map**: `&trait_mod:<is>`, `&infix:<...>`,
   type names.
3. **The `import M` statement** of an inline module (`Stmt::Import` carries no
   lexical set, and an inline package has no parse-time scan).
4. **`require`** (`require_scoped_symbols`): runtime-named symbols.
5. **The block-level module-merge grant** (`MetaNs::ModuleMerge` key): not a
   name at all, but the "this block merged module M's GLOBAL" marker that
   `module_merged_here`, `package_merged_here`, `modules_merged_in_view`,
   `hide_unit_names_from_frame` and `lexical_import_replay` read as an env-tier
   fact. With the restore skipped, exactly three files of `t/` fail, all through
   this grant (`use-in-block-repeated-calls.t`,
   `use-in-block-binds-global-types-lexically.t`, `unit-stash-lists-lexical-types.t`).

Every other kind (plain routines, multis, protos, operators, variables,
constants, scan-known types) already binds lexically.

The env is dynamic: an alias written there is seen by callees of the block and
survives into closures only by accident of capture. The rollback exists to
paper over that, and it cannot be deleted while any of 1-5 still writes the env.

## 2. Decision

**An import never writes the env. Everything it binds is an entry of the
block's lexical import set**, whose chain a name lookup already walks
(`active_lexical_import_chain`, `eval_lexical_import_chain`, `LexImportCall`).

1. **Grant entries.** A lexical import set entry has a third variant
   `LexicalImport::Merge`, keyed by `MetaNs::ModuleMerge.key(module)`: the set
   records that its block merged `module`. `module_merged_here` and
   `package_merged_here` answer "yes" when the running frame's lexical chain
   (the chain the running code was compiled under, cut at the frame the
   `routine_depth` marks: a callee is outside the block, #13801) holds a set with
   that entry, in addition to the existing unit-level tables. The env marker and
   the pop-time removal go.
2. **Type and term entries.** Add `LexicalImport::Term(Value)` (a type object,
   enum, constant, or the code-less term a hook returned). A bareword type
   position the parse-time scan did not resolve compiles to a `LexImportType`
   lookup, the analog of `LexImportCall`, which walks the chain at run time and
   falls back to the existing global resolution when no set binds the name.
3. **Hook maps, `import M`, `require`.** `install_export_symbol`,
   `exec_import_module_op` and `require_scoped_symbols` bind into the innermost
   open set of the importing unit (or the unit's own top-level set, which already
   exists for module units and `EVAL`). `Stmt::Import` gets a set id from the
   compiler exactly as `Stmt::Use` does. A top-level import of the main program
   binds into the unit set, so the pseudo-stash and indirect-lookup readers
   (`MY::`, `::()`, `EVAL`) read the chain and not `imported_env_aliases`.
4. **Deletion.** Once 1-3 hold, delete in the same change:
   `imported_env_keys`, `shadowed_env_values`, `block_bound_keys` (if unused),
   `record_import_env_key`, `restore_import_env_keys`, the `ModuleMerge` env key
   and its `static_link` / `vm_unit_hidden` special cases, and the
   `unit_env_imports` bookkeeping that existed only to restore shadowed values.

Shadowing needs no restore: an inner block's set is earlier in the chain than
the outer one, and leaving the block drops the inner set from the chain.

## 3. Consequences

- A name an import binds is invisible after the block and in callees, as in
  rakudo (`lexical.t`), by construction rather than by rollback.
- A lookup of an unresolved bareword type walks the chain: O(s), s = sets in the
  chain (usually 0-2). It is taken only when neither the pad nor the scan
  resolved the name, so scan-known programs pay nothing.
- `module_merged_here` stays O(1) for code with no lexical sets; otherwise
  O(s). It is memoised per frame by the existing resolution memo.
- Hook exports gain an exact scope: the issue's repro prints `ht` then
  `not visible` without any rollback.

## 4. Rejected

- **Keep the rollback and only fix the three failing files.** The census shows
  four other kinds that still write the env; the rollback would stay, as would
  its leaks into callees.
- **A per-frame "grants" field on `ImportScopeSnapshot` read through
  `import_scope_stack`.** It is dynamic-extent state: a closure created in the
  block and called after it loses the grant, and a callee must be filtered by
  `routine_depth` by hand at each reader. The lexical import set already has
  both properties right.
- **Moving all of it in one change.** The grant (item 1) is independently
  landable and removes the three known dependencies; items 2-4 follow in
  order, each measured with the restore skipped over all of `t/`.

## 5. Open questions

- Whether `LexicalImport::Term` and the enum-key namespace
  (`enum_bare_names`) should share one entry kind.
- Whether a main-program top-level `import M` is a unit-set entry or stays a
  registry-path import (it currently persists for the whole program by design).
