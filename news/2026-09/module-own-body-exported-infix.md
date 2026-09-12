# A module's own body can now call the operator multis it exports

A package (with or without `unit module`) that declares
`multi infix:<OP>(...) is export` made the operator work for its
*importers*, but the module's own routines could not use it: the call fell
through to the core operator as if no user candidate existed at all
(`Cannot resolve caller Numeric(...)`).

## Root cause

Operator scoping is lexical: `user_declared_infix_ops` maps each operator
name to the set of compilation units that declared it, and a call site only
sees the operator when its own executing unit is in that set (an empty set
is the documented escape hatch, "provenance unknown / visible everywhere",
used for exports).

Two bugs combined to break the module-internal case:

1. **`current_unit` was never scoped to a module's own mainline while it
   loaded.** Every per-call site (`vm_call_fast.rs`, `vm_call_light.rs`, …)
   sets `current_unit` from the callee's own declaring file, but a module's
   top-level statements run directly, not through a call — so while
   `multi infix:<*>(...) is export { ... }` executed as a plain top-level
   declaration, `current_unit` was still whatever the *importer* had left it
   as. The operator's own declaration recorded the wrong (importer's) unit
   as its "declaring unit".
2. **The export-time registration was dead code.** `install_export_symbol`
   was meant to force an exported operator's file set to empty ("visible
   everywhere"), but it only filled the entry in *if absent*
   (`.entry(op).or_default()`) — and the entry was never absent, because bug
   1 had already inserted a (wrong) unit for it at declaration time. So the
   "make it visible everywhere" step never actually ran.

With only bug 1 fixed, the module's own body worked (now correctly recorded
as the declaring unit) but *importers* broke, because the file set was no
longer empty and no longer happened to equal the importer's own unit. Both
had to be fixed together: `current_unit` now tracks the module's own
compilation unit for the duration of its mainline (mirroring the existing
`?FILE` save/restore around module loading), and the export-time
registration now genuinely forces the file set empty, so the operator is
visible both inside its declaring module and to every importer.

## Test

`t/modules/import-export/module-export-slash-operator.t` gained a ninth
assertion (`inside-div()`, added to the `SlashOperatorExport` fixture) that
calls the module's own exported `infix:</>` from within the module's own
body — the assertion the file's own comment noted had to be dropped when
that ticket was originally fixed, because of this gap.

Fixes #8008.
