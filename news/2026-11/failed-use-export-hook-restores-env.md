# A `use` whose `sub EXPORT` hook dies restores the importer's env

When a module's `sub EXPORT` died (BinaryHeap rejects an unknown name in
`use BinaryHeap <Foo>`), the importer kept running in the module's own env:
`?` returned before `restore_caller_env_keeping_dynamics`, in the first-load path
(`apply_module_export`) and in the re-import path (`rerun_module_export`). A
closure's captured `@`/`%` variables, which are read from the env by name, then
read as `Nil` after a `try { EVAL "use M <Bad>" }`. The caller's env is now
restored whether the hook returned or died. A `throws-like` over such a `use`
still skips its matcher checks: the `CATCH` handler runs at the throw site with
the module env live (#16968).
