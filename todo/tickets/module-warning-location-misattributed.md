# A module's parse warning is attributed to the importer's line, not its own

A warning raised while parsing a `use`d module (e.g. a "Duplicate 'is
export' trait" warning) prints the *importer's* current line, not the
module's own file and offending line. Rakudo prints the module's file and a
`------>` pointer at the actual line.

```
# lib/PrecompWarn.rakumod
unit module PrecompWarn;
sub pw-hello() is export is export { "hi" }

# pcwarn.raku
use PrecompWarn;
say pw-hello();
```

```
$ mutsu -I lib pcwarn.raku
Potential difficulties:
    Duplicate 'is export' trait
  in block <unit> at pcwarn.raku line 1
hi

$ raku -I lib pcwarn.raku
Potential difficulties:
    Duplicate 'is export' trait
    at .../PrecompWarn.rakumod (PrecompWarn):3
    ------> sub pw-hello() is export <HERE>is export { "hi" }
hi
```

(This used to print twice, once per line above — that duplication is fixed;
see `news/2026-08/module-parse-warning-printed-once.md`. Only the wrong
location remains.)

## Cause

`write_warn_to_stderr` (`src/runtime/runtime_output.rs`) appends its "in ...
at FILE line N" suffix via `self.build_backtrace_string()` — the
*interpreter's current execution position* at print time — not anything
derived from the parser's own warning bookkeeping. The warning fires while
the VM is still mid-executing the `use PrecompWarn;` statement in the
importer, so it shows the importer's line regardless of which parse (or
which file) actually produced the warning.

As of the duplicate-print fix, parse warnings now carry a *file* tag
(`add_parse_warning` snapshots `parser::stmt::simple::parser_source_file()`
at push time; see `PARSE_WARNINGS` in `src/parser/mod.rs`) — but not a
*line* tag. `write_warn_to_stderr` does not consult the file tag either; it
only ever uses `build_backtrace_string()`.

## Why this is not a one-liner

Fixing this needs two things:

1. **A line, not just a file, recorded per warning.** `add_parse_warning`
   currently takes only a message string; giving it (or its ~10 call sites in
   `param_validate.rs`, `traits.rs`, `sink_warn.rs`, `number.rs`,
   `colonpair.rs`, `simple.rs`, `has_decl.rs`, `pointy_param.rs`,
   `param_inner.rs`) access to the current parse position would require
   either touching every call site to pass a line number, or teaching the
   parser to track "current line being parsed" as a thread-local the way it
   already tracks "current file" (`parser_source_file()`) — plausible, but a
   distinct, larger change from the file-tag fix that closed the duplicate
   print.
2. **The print sites (`write_warn_to_stderr` and its callers in `run.rs`,
   `run_modules.rs`, `system_eval_string.rs`, `builtins_system_require.rs`,
   `runtime_output.rs::emit_parse_warnings`) preferring the warning's own
   recorded file+line over `build_backtrace_string()`'s current-execution
   position when one is available.** Rakudo's format for this case is also
   different from a runtime warning's backtrace — it shows the source
   snippet with a `------>` pointer (`at FILE (Module):LINE` +
   `------> ... <HERE> ...`), not `in block <unit> at FILE line N`. Matching
   that format is itself nontrivial and shared with none of the existing
   `write_warn_to_stderr` call sites.

Whoever picks this up should start from `emit_parse_warnings` in
`src/runtime/runtime_output.rs` (already carries the file tag per warning)
and figure out how to thread a line number alongside it from `add_parse_warning`.
