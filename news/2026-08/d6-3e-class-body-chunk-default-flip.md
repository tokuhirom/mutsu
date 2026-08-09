# ADR-0019 D6-3e: class-body statement chunks run by default

`run_class_body`'s small statement arms (`class_body_other_stmt`,
`class_body_code_alias`, `run_class_body_leave_phasers`) now run each
statement's precompiled `body_plan` chunk (ADR-0019 D6-3a-d) by default,
instead of on-the-fly compiling the raw statement via `run_block_raw` on
every class registration. Previously this path only ran under the
`MUTSU_DROP_LEGACY_CLASS_BODY=1` instrument, added in D6-3d to validate the
chunk mechanism before flipping it on. `token`/`rule` statements keep their
existing `run_block_raw` execution unchanged — they never get a compiled
chunk (the ADR-0009 carve-out), so `run_class_body_chunk_or_raw` falls
through to the same branch as before for them.

Since the instrument had already been forced through a full verification
sweep in D6-3d, this is a pure default flip with no behavior change,
re-confirmed by the same sweep: the full `t/` suite (28,062 tests), the
`S12-class`/`S12-construction`/`S14-roles`/`S05-grammar` roast files (957
tests, only the pre-existing non-whitelisted `S12-class/open_closed.t`
failure), and `scripts/battery-testsuite.sh` (158/164 files pass, 2
excluded, byte-identical to the D6-3d baseline).

`CompiledClassDeclPlan::legacy_body` itself is not dropped yet (ADR-0019
D6-4): `run_class_body`'s dispatch loop still needs the raw flattened
`body: &[Stmt]` to classify each statement's kind, and the `Attr`/`Method`/
`Does` arms still take a raw `&Stmt` for their own logic and fallback
paths. Removing the field requires threading those three handlers onto
`ClassBodyOp`'s already-typed fields instead.
