# An EVAL that uses a module and starts a thread no longer trips a debug assertion

`Interpreter::close_import_region` had `debug_assert!(false, "an import region's env was replaced
wholesale")`, with the note that no test reached it. Running a `t/` file as `EVAL(slurp($f).AST)`
reached it for 410 files on a debug build: any EVAL whose unit `use`s a module and then spawns a
thread (`start`, `Thread.start`). Spawning flattens a scoped env, which folds the open import region
into the one flat tier, so the region is gone when the unit ends. That is the expected end of such a
region: its imports are plain by-name writes by then, and the unit's own bookkeeping
(`unit_env_imports`, the alias removal in `with_eval_import_scope`) takes them out again. The
assertion now fires only when the env was replaced while the region's layer is still in the chain,
which is a real mismatch. A string `EVAL` had the same assertion failure, so this is not specific to
`EVAL($ast)`. Release builds already behaved correctly: the import does not leak out of the EVAL.
