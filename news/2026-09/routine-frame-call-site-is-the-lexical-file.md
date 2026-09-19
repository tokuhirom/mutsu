# A routine frame's call site is the caller's own lexical file, not `?FILE`

A `RoutineFrame`'s call-site `file` was the dynamically-scoped `?FILE` env
value. `run_modules.rs` scopes `?FILE` to a module's own path only while that
module's *mainline* is loading, restoring the importer's value once loading
returns — so once a `use`d module's own routines started calling each other,
every call site inside that module was recorded under the *importer's*
script, with the module's own line numbers. The first CPU profile of
`benchmarks/bench-json-fast.raku` showed exactly this: every call site inside
`JSON::Fast` filed under `bench-json-fast.raku:275`, a script only 84 lines
long.

Fixed by resolving a call site's file the same way `Interpreter::
executing_source_file` already resolves "what file is running right now" for
`callframe()`/`CALLER::`: an outward walk over the live `routine_stack` for
the nearest frame that names its own declaring file (`def_file`), rather than
trusting `?FILE`. A new `Symbol`-typed twin,
`Interpreter::executing_source_file_sym`, is now what every
`push_routine_with_location`/`push_method_routine_with_location`/
`push_block_routine_with_location` call site uses instead of
`current_source_file_sym`.

The same investigation found a second, closely related gap: an inlined bare
block (`try { ... }`, a lone `{ ... }` statement) records no `def_file` of
its own — by design, it belongs to whichever routine lexically encloses it —
so its backtrace frame fell straight through to the same stale `?FILE`, even
though the *enclosing* routine's own frame reported correctly (masked by its
own `def_file`). `build_backtrace_value_with_leading` and
`build_backtrace_string` in `vm_helpers.rs` now resolve the innermost frame's
file through `executing_source_file`/`executing_source_file_sym` too.

With the source-level fix in place, the profiler's own reconciliation pass
(`ProfileAggregate::resolve_caller_files`, an outward walk over the *sampled*
stack reconstructing the same answer after the fact) is redundant and is
deleted, along with the `caller_file` parameter it fed into
`profile::record_routine_frame`.

Pinned by `t/modules/backtrace-module-block-file.t` (a module's own inlined
block reports the module in its backtrace, not the script that `use`d it,
matching `raku`), a new Rust unit test suite for `executing_source_file_sym`
in `src/runtime/accessors_stack.rs`, and a profiler-level regression test in
`src/profile/aggregate.rs`.

Closes [#8743](https://github.com/tokuhirom/mutsu/issues/8743).
