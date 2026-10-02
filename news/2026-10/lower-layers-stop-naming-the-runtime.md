# The lower layers stop naming the runtime, and a ratchet keeps it that way

`mutsu` is one ~730k-line crate, and it cannot be split into crates because its layers reference
each other in a cycle: the parser and `Value` name the runtime, the VM and the builtins, which
name them back (issue #10779). Many of those upward edges are pure helpers or tables that just
live in the wrong module.

`make check-layer-deps` (part of `make checks`, and a CI step) counts every `crate::runtime` /
`vm` / `compiler` / `builtins` / `trir` / `Interpreter` path in the lower layers — the AST, the
parser, `Value`, `opcode`, `Env`, the GC, and the name/key leaf modules `symbol`, `qualified`,
`meta_ns`, `str_scan` and `type_id`. The parser counts as upward from below it too. Per-file
counts live in `scripts/layer-deps-baseline.txt` and may only go down.

The first moves took the count from 205 to 189:

- `MetaNs` moved from `src/runtime/meta_ns.rs` to `src/meta_ns.rs`, and the name-marker byte
  scans (`has_double_colon`, `has_routine_scope_marker`, ...) from `src/runtime/utils/` to
  `src/str_scan.rs`. Both depended only on `symbol` and `env` already.
- `is_routine_scoped_implicit_var` moved into `src/symbol.rs`, the one lower module that calls
  it.
- `StateScopeGuard` (a field of `SubData`) moved into `src/value/state_scope_reaper.rs`.
- The `Buf`/`Blob` class-name predicates moved to `src/value/buf_class_names.rs`, and
  `value_type_name` to `src/value/type_name.rs`.

`runtime::utils` re-exports the moved functions, so the runtime and the VM call them as before.
Nothing changes in behavior.
