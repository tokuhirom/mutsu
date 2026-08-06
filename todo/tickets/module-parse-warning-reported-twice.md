# A module's parse warning is reported twice

A warning raised while parsing a `use`d module is printed twice. Rakudo prints
it once.

```
# tmp/pclib/PrecompWarn.rakumod
unit module PrecompWarn;
sub pw-hello() is export is export { "hi" }

# tmp/pcwarn.raku
use PrecompWarn;
say pw-hello();
```

```
$ mutsu -I tmp/pclib tmp/pcwarn.raku
Potential difficulties:
    Duplicate 'is export' trait
  in block <unit> at tmp/pcwarn.raku line 1
Potential difficulties:
    Duplicate 'is export' trait
  in block <unit> at tmp/pcwarn.raku line 1
hi

$ raku -I tmp/pclib tmp/pcwarn.raku
Potential difficulties:
    Duplicate 'is export' trait
    at .../PrecompWarn.rakumod (PrecompWarn):3
    ------> sub pw-hello() is export <HERE>is export { "hi" }
hi
```

## Cause

The module's source is parsed twice, and both parses drain `PARSE_WARNINGS` into
stderr:

1. at the importer's *parse* time, by the export scan
   (`parser::stmt::simple::module_exports::extract_exported_names`, which calls
   `parse_program_partial`), and
2. at *run* time, by `Interpreter::parse_module_source`
   (`src/runtime/run_modules.rs`).

Whoever drains second sees a fresh set from its own parse, so both print.

Found while making the precompilation cache replay parse effects
(`news/2026-07/precomp-cache-replays-parse-effects.md`). That change made the
count *consistent* between a cold and a warm cache — it is two either way now,
where it used to be two cold and one warm — but did not address the duplication
itself.

Note the second difference visible above: mutsu attributes the warning to the
*importer's* line (`at tmp/pcwarn.raku line 1`) rather than to the module file
and offending source line. Both are worth fixing together, since they come from
the same place — the warning carries no origin, so it is stamped with whatever
location is current when it is drained.

## Why this is not a one-liner

Simply suppressing the export-scan drain would lose warnings from a module that
is only ever scanned and never loaded at run time. The warnings need an origin
(source path + line) recorded when they are raised, and de-duplication on that
origin — which also fixes the misattributed location. That means touching
`add_parse_warning`'s signature and every one of its ~10 call sites.

## Investigation notes (2026-08-06)

Traced the exact call chain with `rust-gdb` breakpoints on `write_warn_to_stderr`
for the repro above:

- **First print**: `Interpreter::run` (`src/runtime/run.rs:397`) — the mainline
  parse of `pcwarn.raku` itself. Parsing its `use PrecompWarn;` statement
  triggers `scan_module_source` -> `parse_program_partial` on the module (for
  the export scan), which pushes the warning into the *global* `PARSE_WARNINGS`
  thread-local but never drains it. When the outer `parse_program` finishes and
  `Interpreter::run` calls `take_parse_warnings()`, it picks up this leftover
  entry along with (none, in this repro) its own.
- **Second print**: `Interpreter::parse_module_source`
  (`src/runtime/run_modules.rs:448`) — executing the `use` opcode actually loads
  the module, re-parsing the identical source fresh and generating the same
  warning text again, drained and printed here too.

**The location misattribution has a different root cause than "no origin
tracked at push time".** `write_warn_to_stderr` (`src/runtime/runtime_output.rs`)
appends its "in ... at FILE line N" suffix via `self.build_backtrace_string()`
— the *interpreter's current execution position* at print time, not anything
derived from the parser's warning bookkeeping. Both prints fire while the VM is
still mid-executing the `use PrecompWarn;` statement in `pcwarn.raku`, so both
show `pcwarn.raku line 1` regardless of which parse actually produced the
warning. Fixing this needs the print sites to use the warning's own recorded
origin instead of the backtrace when one is available — a separate change from
whatever fixes the duplication.

**Attempted approach and why it was not landed:** `parser_source_file()`
(`src/parser/stmt/simple/lib_paths.rs`) already tracks the file currently being
parsed and is readable with zero call-site changes (`add_parse_warning` can
snapshot it internally) — so *file*-level dedup (skip a `(file, message)` pair
already surfaced) is cheap and closes the observed bug without the full
per-line plumbing. The blocker is the **reset boundary**: `parse_program`
(`src/parser/mod.rs:314`) clears `PARSE_WARNINGS` at the start of *every*
top-level parse, including each nested module/EVAL parse — so a
session-persistent "already surfaced" set must NOT be cleared there, or the
module's second (run-time) parse would just refill it and reprint (defeating
the fix). But it is unclear whether the set should ever reset at all within one
process: mutsu tests commonly spin up multiple `Interpreter`s or run several
`.t`/EVAL'd programs in one process (`is_run`, nested EVAL, the REPL), and each
such *separate program run* likely should see its own warnings independently,
the way separate `raku` process invocations would. Getting this boundary wrong
either re-introduces the duplicate (reset too often) or silently suppresses a
legitimate warning in a later, unrelated script sharing the process (reset too
rarely) — and no existing test pins which behavior is correct, so a wrong
choice would not necessarily be caught by `make test`/`make roast`. Whoever
picks this up should either find/add such a test first, or scope the "surfaced"
set to something narrower than the whole process (e.g. per top-level
`Interpreter::run` invocation, threaded through explicitly rather than a bare
thread-local) so nested module/EVAL parses share it but sibling top-level runs
don't.
