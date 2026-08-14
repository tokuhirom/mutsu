# A module's parse warning is no longer printed twice

A warning raised while parsing a `use`d module — e.g. `sub foo() is export
is export { ... }`'s "Duplicate 'is export' trait" — used to be printed
*twice*, where Rakudo prints it once:

```
$ mutsu -I lib script.raku
Potential difficulties:
    Duplicate 'is export' trait
  in block <unit> at script.raku line 1
Potential difficulties:
    Duplicate 'is export' trait
  in block <unit> at script.raku line 1
```

## Root cause

mutsu parses a `use`d module's source twice: once during the importer's
*parse-time* export scan (`parser::stmt::simple::module_exports::scan_module_source`,
which needs to know what a module exports before the importer's own parse can
finish), and again at *run time* when the `use` statement actually executes
and loads the module (`Interpreter::parse_module_source`). Each of those two
parses independently drains the parser's collected warnings and prints them,
so a warning the module's source raises came out twice — and, since a
precompilation-cache hit replays the same warnings a third way, the count was
consistent between cold and warm caches (both two) rather than the caches
disagreeing, but the duplication itself was never addressed.

## Fix

Parse warnings are now tagged with the file being parsed at the moment each
was raised (`add_parse_warning` snapshots `parser_source_file()` internally —
no call-site changes needed at any of its ~10 call sites). The `Interpreter`
tracks which `(file, message)` pairs have already been surfaced during the
current top-level `run()` and skips re-printing one it has already shown.

Getting the *scope* of that tracking right was the actual crux of the fix:

- It must **not** reset on every nested parse (a module's own `parse_program`
  call clears the *unrelated* raw warning buffer at the start of every parse,
  including nested ones) — resetting the surfaced-warnings tracking there too
  would just let the module's second, run-time parse refill it and reprint.
- It must **also not** live for the whole process. mutsu's REPL and several
  test harnesses call `Interpreter::run()` more than once on the *same*
  `Interpreter` (each REPL line is its own top-level compilation unit); a
  tracking set that persisted across those calls would silently swallow a
  legitimate warning on a later, unrelated line that happens to produce
  identical warning text to an earlier one.

The tracking is therefore a field on `Interpreter`, reset at the very top of
`run()` — shared by everything that executes *within* one top-level run
(mainline parsing, nested module loads, `EVAL`, `require`), but starting
fresh for the next one. A new test,
`repl_core::tests::test_parse_warning_dedup_does_not_leak_across_repl_lines`,
pins that boundary directly: it feeds the *same* warning-triggering
declaration on two separate REPL lines and asserts the warning fires on
both — regression-testing exactly the failure mode described above (verified
by temporarily removing the reset and watching the test catch it).

`scan_module_source` (the export scan) previously left the currently-parsed
file untouched during its nested parse, so a warning it raised was
(mis)tagged with the *importer's* file rather than the module's own — which
would have silently defeated the file-tagged dedup, since the module's
run-time parse tags its own warnings correctly. It now wraps the nested parse
with `set_parser_source_file`/restore, matching the pattern already used by
the run-time module loader and `require`, so both parses of a module tag its
warnings with the same file. `require`'s own-file parse gained the same
wrapping for consistency (it previously left the requirer's `$?FILE` in
place for the required file's parse). File-tag comparison canonicalizes both
sides (`std::fs::canonicalize`, falling back to the raw string) so the
parser's own module resolver and the runtime's independent module resolver
agreeing on a file's *identity* is enough — they need not render its path
identically.

Location misattribution (the printed warning shows the importer's line
instead of the module's own offending line) is a separate, still-open bug —
see `todo/tickets/module-warning-location-misattributed.md`.

Regression coverage: `t/module-parse-warning-once.t` (a fresh fixture module
under `t/lib/` with a deliberately duplicated `is export` trait, checked
against both a cold and a warm precompilation cache) and the REPL unit test
above.
