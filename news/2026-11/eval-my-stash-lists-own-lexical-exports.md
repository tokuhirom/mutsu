# `MY::` inside an EVAL lists a module's `my sub ... is export` routines

A `use` inside an `EVAL`'d unit binds the module's lexical exports in the unit's
import sets. `MY::` read only the code objects of those sets and skipped the
routines recorded as definitions, so `MY::<&own-plain>` of a `my sub own-plain() is
export` was missing (the top level, which reads the visible routines, had it). The
sets' routine bindings are now listed too, so `export-own-subs-with-reexported-stash.t`
passes as `EVAL($ast)`.
