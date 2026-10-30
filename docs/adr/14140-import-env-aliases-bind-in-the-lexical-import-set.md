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
   (including `import M`) is decided to bind into the unit set, so the pseudo-stash and indirect-lookup readers
   (`MY::`, `::()`, `EVAL`) read the chain and not `imported_env_aliases`.
4. **Blocks with no `use` of their own set: `need`, conditional `use`,
   `import`, `use ... :from`.** Today `Compiler::use_set_id` names a set only
   for an unconditional `Stmt::Use` of a lexically importable module. A block
   whose only imports are a `need` (or `use M if COND`, an inline-module
   `import M`, `use M:from<...>`) still opens an `ImportScope`, and the module
   merge grant it makes is the *only* state it leaves in the env. Deleting the
   env marker without a home for that grant would either lose it (`need M;
   M::Foo` stops resolving in the block) or put it in an outer set, leaking it
   past the block. So every import-carrying statement, whatever its kind, gets a
   *grant set*: the compiler names one per statement (id = statement kind,
   module, tags, a per-site ordinal for a conditional one), opens it in
   `enter_lexical_use_block` like any other set, and the statement fills it when
   it runs (a conditional `use` fills it only when its condition held). The
   grant is an entry of that set (`LexicalImport::Merge`), nothing else.
   A closure created in the block carries the set in its chain, so it keeps
   the grant after the block exits, while a callee written outside the block
   does not have it (#13801).
5. **Deletion.** Once 1-4 hold, delete in the same change:
   `imported_env_keys`, `shadowed_env_values`, `block_bound_keys` (if unused),
   `record_import_env_key`, `restore_import_env_keys`, the `ModuleMerge` env key
   and its `static_link` / `vm_unit_hidden` special cases, and the
   `unit_env_imports` bookkeeping that existed only to restore shadowed values.

Shadowing needs no restore: an inner block's set is earlier in the chain than
the outer one, and leaving the block drops the inner set from the chain.

### Tests that hold the visibility expectations

The implementation is verified against these (each reads a grant or a bound
name across a block, a closure or a callee):
`t/modules/import-export/use-in-block-repeated-calls.t`,
`use-in-block-binds-global-types-lexically.t`,
`t/modules/unit-stash-lists-lexical-types.t` (the three that fail when the
restore is skipped), and for the `need` path
`need-after-load-export-stash.t`, `need-populates-export-stash.t`,
`need-preserves-nested-use-imports.t`, `need-bare-module-multi-not-imported.t`,
`need-bare-module-plain-sub-not-imported.t`,
`need-file-module-our-code-var-stays-private.t`,
`t/modules/module-global-merge-is-lexical.t`,
`use-merges-packages-into-my-not-global.t`, plus `roast/S11-modules`.

## 2.1 Where the grant lives at run time: a frame-record field

The leaf readers of the grant (`module_merged_here`, `package_merged_here`,
`modules_merged_in_view`, `hide_unit_names_from_frame`, `lexical_import_replay`)
are run-time gates reached from dynamic name resolution. They have no source
position, so a compile-time chain index cannot reach them, and a set entry alone
cannot say whether its block is still open (a set is global by id). The grant
needs (a) a region that opens and closes with the block, (b) inheritance by
closures and routines created inside the region, (c) invisibility to a callee
written outside it (#13801). Options:

| Option | (a) region | (b) closure capture | (c) callee | Cost per read | Cost per closure creation |
| --- | --- | --- | --- | --- | --- |
| A. A field of the frame record (`FrameFixtures`, ADR-14843) | scoped helper opens and cuts it, as `begin_scoped_invocant` does | `capture_fixtures` already copies the creator's record into `SubData`/`LazyList` | a call pushes its own record, so the callee has none | O(g), g = granted modules in the record (usually 0-2), one record read | O(1) extra: the record is already copied, the grant list is an `Arc` bump |
| B. A new `SubData` field | the region still needs its own stack | explicit, but a second capture mechanism beside `fixtures` | needs a second per-call push | O(g) | one more field on every closure |
| C. The compile-time chain of the creating code | none: the chain is static, so a statement after an inline block sees the block's set | free | free | O(s) | O(1) |
| D. Set entries plus a run-time "open" flag | a flag per set, restored on every exit path | none: a closure would read the flag after exit | wrong: the flag is global | O(s) | none |

**Recommendation: A.** `FrameFixtures` gains `import_grants:
Option<Arc<[Symbol]>>` (the modules the open block regions of this frame, and of
the frame a closure was created in, merged). `OpCode::ImportScope`'s open and
close edit it through a scoped helper, the same way a rebinding of `self` is
scoped: pushed at the region's entry, cut on every exit path (return, exception,
non-local exit). `merge_module_into_importer` adds to the innermost record
instead of writing an env key. The readers ask the current record. The grant set
of §2.4 stays the compile-time declaration of *which* statements grant (and the
home of the other entries); the record is the run-time extent. Lifetime: a
closure that escapes keeps its `Arc`, which dies with the closure; nothing is
global. Consistency with ADR-14843: the record is already the authority for the
frame's fixed names, copied into closures and pushed with the frame, so the
grant adds no second capture path, and `make check-frame-fixtures` extends to it.

Known cost of A: a named `sub` declared in the block and called after the block
exited carries no record unless routine values capture one; whether
`capture_fixtures` already covers them is verified before implementation, and if
not, the grant for such a routine falls back to the compile-time chain of its
declaration (option C, which is exact for a routine because its body is one
code object). Open to the maintainer: whether the record or the closure should
own the `Arc`.

### Findings that qualify the recommendation (found while sizing the implementation)

1. **Records are not uniform per frame.** `FrameFixtures` exist for method
   frames and scoped helpers; a plain function frame has none, and a callee
   without one reads its *caller's* top record (hence the `fixtures != 0`
   guards). A grant stored there needs the same guards plus a record for every
   frame that runs an `ImportScope` region, including the main program.
2. **A routine called by name has no creation link.** `{ use M; sub f { M::Foo }
   f() }` calls `f` through the registry by `FunctionDef`; there is no closure
   value to capture a grant, so a frame-cut rule (`routine_depth`) hides the
   grant from `f`, while today's env key reaches it through the block's static
   link. Option A therefore needs a region handle on registry-declared routines
   too, which `FunctionDef` (shared across executions of the block) cannot hold.
3. **The env key already is the lexical pad binding.** Its static-link behavior
   (visible to nested closures and routines, invisible to callees, kept by an
   escaping closure) is what ADR-8751 asks for. What is wrong is only that the
   binding lives in the *dynamic* env without a tier of its own, so it needs a
   key list and a restore. The alternative to A is therefore an *import tier*
   owned by the `ImportScope` region (an `Env` layer opened with the region and
   dropped with it, never merged outward), which keeps the static-link behavior
   and deletes `imported_env_keys` / `shadowed_env_values` / the restore. It is
   unmeasured: `BlockScope` already opens a tier (`open_block_env_tier`) whose
   exit merge propagates package-qualified names, package type objects and
   uppercase names, and the region's tier would have to keep those rules for
   non-import writes.

Until a spike measures the import tier against the 13 tests listed above, option
A is **not** the recommendation to implement; the choice between A (with region
handles on routines) and the import tier is the open decision for the maintainer.

## 2.2 Spike result: the grant in a region-owned env tier

A spike (branch `spike/14140-import-tier`, not for merge) moved only the
module-merge grant. `OpCode::ImportScope` opens `open_block_env_tier` with the
region and, on exit, merges every tier write except the `ModuleMerge` keys back
into the enclosing env; the grant no longer enters `imported_env_keys` /
`shadowed_env_values`. The scope keeps a small `merged_modules` set (for
`hide_unit_names_from_frame`, `MY::` and the replay record), nothing else.

Measured on a debug build, 2026-10-30:

- **(a) Visibility tests.** With the grant out of the key list and no restore
  for it, all of `t/` passes (about 9,100 files; five files timed out only under
  a 6-way parallel run and pass alone), including the three files that fail
  when the restore is skipped, plus the 15 whitelisted `roast/S11-modules`
  files. The three files read the grant through the static link, which the tier
  keeps.
- **(b) Cost.** One tier per executed region (an `Env` handle clone and one
  `Arc`). A loop of 3,000 `{ use M }` regions took 0.34-0.46 s on the spike and
  0.34-0.53 s on `main`: no difference above the noise. This is wall clock, not
  callgrind `Ir`; the `Ir` measurement is still owed before an implementation PR.
- **(c) A routine called by name inside the block.** `{ use M; sub f { ::('T')
  } f() }` gives `none` on both `main` and the spike; rakudo gives the type.
  The tier changes nothing here, because the gap is the registry call having no
  link to the block's pad (#15661), not the grant's storage.

What the spike does not show: the other import kinds (types and terms the scan
cannot see, hook maps, `import M`, `require`) still write the env and the key
list. They need the same tier plus an exclusion rule per kind (the tier merge
must keep non-import writes), which the grant alone does not exercise.

**Updated recommendation: the import tier (§2.1 option "tier"), not option A.**
It keeps the static-link behavior the existing tests pin, needs no per-frame
record and no capture on closures or routines, and the grant slice is a small
independent change (the spike diff is about 70 lines). Option A stays rejected
for the reasons in the findings above. Open to the maintainer: whether the next
slice moves the other import kinds into the same tier by exclusion (keep every
non-import write) or by an explicit import layer below the block tier.

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
