# ADR-0019 D6-4: dropped CompiledClassDeclPlan::legacy_body, closes D6

`run_class_body` now iterates the compiler-precomputed `body_plan:
&[ClassBodyOp]` directly instead of a raw, separately-stored
`Vec<Stmt>`. Previously the registration-time walker zipped a
`SyntheticBlock`-flattened, nested-`has`-decl-appended copy of the class
body against `body_plan` on every registration, re-deriving at runtime
exactly what the compiler had already computed once at plan-lowering time.
`body_plan` is built by the identical flatten-then-classify-then-append
transform, so it already carries every op in the same order — the
runtime-side preprocessing was pure duplicated work.

Two of the small per-statement handlers changed shape to stop needing a
raw `Stmt` at all: `class_body_does_decl` now takes the `Does` op's own
`name: Symbol` field directly instead of re-matching `Stmt::DoesDecl` for
it, and `ClassBodyOp::Attr` gained its own `raw: Stmt` field (populated
unconditionally, the same way `ClassSub`/`CodeAlias`/`ProtoMethod`/
`LeavePhaser` already carry theirs) so `class_body_has_decl`'s existing
fallback for a class-level `our`/`my` attribute — excluded from the
compiler's name-keyed `attr_decls` table by design, so it needs the raw
`HasDecl` statement to build an ad hoc descriptor — still has one to read,
without a separate lookaside list to keep in sync.

With every consumer moved onto `body_plan`, `CompiledClassDeclPlan::legacy_body`
itself, its one construction site, and `register_class_decl`'s now-unused
`body: &[Stmt]` parameter (plus its three call sites' trailing argument)
are all deleted. This closes ADR-0019's D6 box.

Verified via the full `t/` suite (28,062 tests), all 701 Rust unit tests,
the `S12-class`/`S12-construction`/`S14-roles`/`S05-grammar` roast files
(957 tests, only the pre-existing non-whitelisted `S12-class/open_closed.t`
failure), `scripts/battery-testsuite.sh` (158/164, byte-identical to
baseline), and a hand comparison against `raku` exercising every
`ClassBodyOp` variant in one class declaration (an attribute with an
`our`/`my` sibling to force the new fallback path, `also does`, a
class-scoped `sub`, a code alias, a `proto method`, and a `will leave`
phaser) — byte-identical output.
