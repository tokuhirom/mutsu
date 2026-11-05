# A call of a lexically imported routine is served from a per-site memo and no longer keeps its chunk out of the JIT

A call of a routine imported by a top-level or block `use` ran through
`exec_lex_import_call_site` on every call: two hash lookups into the lexical
import sets and an `Arc<FunctionDef>` clone to find the routine the name is
bound to. The chunk now remembers that definition per call site
(`CompiledCode::lex_import_sites`, `LexImportSiteCaches`), valid for one value
of `ModuleState::lexical_import_gen`, which is bumped at the single place
`lexical_import_sets` is written (`finish_lexical_import_capture`) and starts in
a range of its own per interpreter. A name that is a built-in operator's
candidate family (`:<`) or that a set binds as a code object is never
memoized, so the operator merge still sees every call.

The op also made the whole chunk JIT-ineligible: `LexImportCall` consumes the
op after it, which the straight-line op tables cannot express, so a loop that
called an imported routine ran in the interpreter loop. The JIT now compiles
it: the shim returns `JIT_STATUS_SKIP` when it ran the call at `ip + 1` itself,
and the emitted code branches to `ip + 2` (admitted only when the next op is
`CallFunc`, `CallOnCodeVar`, `GetCodeVar` or `GetBareWord`).

`t/modules/import-export/use-in-block-imported-call-site-memo.t` pins that the
memo follows a rebinding (two blocks, an inner `use`, a `use` re-run per loop
iteration). Part of #14371.
