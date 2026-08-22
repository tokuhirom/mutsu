# Class/role declarations no longer pay two throwaway method-body compiles

ADR-0019 D3-8 made the main-pass compiler compile every class/role method body
exactly once, so registration could install that bytecode instead of compiling
the body again. Two duplicate compiles survived that cutover on either side of
it, and both are now gone. This closes
`todo/deep/adr0019-hoisted-type-shell-throwaway-method-compile.md` and the last
open item of `todo/tickets/adr0019-method-body-compile-dedup-remnants.md`.

## 1. The hoisted forward-reference shell's registration-time compile

`hoist_type_decl_shells` (`src/compiler/helpers_ast_utils.rs`) pre-registers a
declaration-only `__hoisted` shell for every class/role declared after the first
runtime statement in its block, so an earlier mainline statement can already
name the type. `add_class_decl_plan` / `add_role_decl_plan`
(`src/compiler/decl_plan.rs`) deliberately compute `package_name: None` for such
a shell and leave every `CompiledMethodDecl::compiled_routine_key` `None` — the
shell's plan is superseded moments later by the real, source-position
declaration, so compiling its bodies at compile time would be redundant.

Skipping the *key*, however, did not skip the *runtime* work.
`exec_register_class_op` / `exec_register_role_op` (`src/vm/vm_typedecl_ops.rs`)
still called `compile_class_methods` / `compile_role_methods` unconditionally
for the shell's own registration pass. With every key `None`,
`class_body_method_decl` / `role_body_method_decl`'s `matched_compiled_fn`
lookup always missed, so every method on the shell got the full pre-D3-8
registration-time compile (`compile_method_def_in_place_with_dist`) — and the
whole `MethodDef` set it produced was discarded wholesale when the real
declaration re-registered the type from its own, properly keyed plan. Nothing
read the compiled code in between: it was 100% wasted work, paid by essentially
every non-trivial OO file (a `t/*.t` opening with `use Test; plan N;` already
trips the hoist for every type declared below it).

The `todo/deep` entry expected this to need new plumbing (a `skip_method_compile`
flag threaded from `is_hoisted_shell` through `CompiledClassDeclPlan` /
`CompiledRoleDeclPlan`). It does not: both plans already carry `custom_traits`,
and the class site was already reading `__hoisted` out of it to build
`ClassDeclModifiers::is_hoisted_shell`. Both registration sites now skip that
pass for a `__hoisted` shell. The skip is
safe rather than merely cheap: if a forward reference really does call a method
on the shell-registered type before the real declaration runs, two independent
on-demand compiles already cover it — `populate_uncompiled_method`
(`src/vm/vm_call_method_compiled_cache.rs`), which compiles into the canonical
registry, and `run_resolved_method_celled`'s `compiled_holder` fallback
(`src/runtime/class_dispatch.rs`).

Measured with the `MUTSU_VM_STATS=1` counter
`adr0019-d3-8: method_body_runtime_compiles`, which counts exactly these
registration-time compiles:

| corpus | before | after |
| --- | --- | --- |
| `say "hello"; class Foo { method a {…}; method b {…}; method c {…} }` | 3 | 0 |
| `say "hello"; role R {…}; class C does R {}` (2 role methods) | 4 | 0 |
| `t/role-pun-build-tweak.t` | 21 | 7 |
| `t/text-csv-battery.t` | 150 | 95 |
| whole `t/` suite (3335 files, summed) | 2376 | 278 |

No file's count went up. The 278 that remain come from other, unrelated
mechanisms (`augment`, parametric role punning, proto-method redispatch,
`EXPORTHOW` declarators) and are out of scope here.

## 2. `record_type_body_captures`'s analysis-only second compile

`record_type_body_captures` (`src/compiler/helpers_sub_body.rs`) ran a full
`compile_closure_body` per top-level method of every class/role body, purely to
harvest `free_var_writes` / `free_var_container_writes` /
`needs_cell_named_sub_free` into `CompiledCode::type_body_written_lexicals`,
then threw the compiled code away. That set is what keeps a frame lexical a
method writes on the name-keyed `shared_vars` lane — a method has no runtime
creation op, so `box_captured_lexicals` never sees it (pins:
`t/destroy-cross-thread-writeback-coherence.t`,
`roast/S12-construction/roles-6e.t`).

Two earlier investigations left this open because the two compiles use
deliberately different compiler contexts: the analysis compile runs on the
OUTER, main-pass `Compiler` (full enclosing lexical scope), while
`compile_method_body` uses a bare `Compiler::new()` that must stay scope-blind
to keep byte-parity with the registration-time compile it replaces. The
assumption was that outer-scope visibility is what makes the harvest correct.

It is not. `free_var_writes` and friends are computed by
`CompiledCode::compute_free_vars`, a post-compile pass that partitions names
purely by whether the compiled body owns them as locals — a fact of the body
itself, not of the enclosing compiler's scope. So the scope-blind compile yields
the same set. That was checked rather than assumed: an env-gated instrumented
build computed both harvests side by side and reported every difference across
the whole `t/` suite (3335 files) and the entire roast whitelist (1436 files).
Out of those 4771 files there were 7 divergent lines in 4 files (3 under `t/`,
`roast/integration/advent2013-day06.t`), and every one of them was
compiler-minted temporaries (`__mutsu_call_result_N`, `__mutsu_index_rw_arg_N`,
`__mutsu_destructure_snap_N`, ...) differing only in their ordinal — those are
`_`-prefixed, so the existing "genuine `my` lexical" filter drops them on both
sides. Post-filter the two harvests are identical everywhere.

`compile_method_body` therefore now records the captures as a byproduct of the
one compile it already performs, and `record_type_body_captures` is gone from
`Stmt::ClassDecl` / `Stmt::RoleDecl` compilation. Coverage is preserved at the
edges:

- A method the main pass does not compile — one whose own name is computed
  (`method ::($n) {…}`), or any method of a `class ::($n) {…}` — falls back to
  `record_type_body_captures_uncompiled`, the same analysis-only compile under a
  narrower name.
- A `__hoisted` shell records nothing at all now (it previously also recorded
  nothing, since the hoist path never called `record_type_body_captures`); the
  real declaration's pass writes the identical set into the same
  per-`CompiledCode` vec.
- The harvest moved into the `SyntheticBlock`-flattened walk
  `compile_method_body_keys` already does, so it now sees methods the old
  top-level-only walk missed — strictly more conservative.

Five unit tests in `src/compiler/helpers_method_body.rs` pin the harvest
(hoisted class, unhoisted class, role, computed-name-class fallback, and the
temporaries filter).
