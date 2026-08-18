# A module's parse warning now names its own file and line, not the importer's

A warning raised while parsing a `use`d module (e.g. a "Duplicate 'is
export' trait" warning) used to print the *importer's* current execution
line, not the module's own file and offending line:

```
$ mutsu -I lib pcwarn.raku
Potential difficulties:
    Duplicate 'is export' trait
  in block <unit> at pcwarn.raku line 1
```

## Root cause

`write_warn_to_stderr` (`src/runtime/runtime_output.rs`) appended its "in
... at FILE line N" suffix via `build_backtrace_string()` — the
*interpreter's current execution position* at print time, not anything
derived from the parser's own warning bookkeeping. The warning fires while
the VM is still mid-executing the `use PrecompWarn;` statement in the
importer, so it showed the importer's line regardless of which parse (or
which file) actually produced the warning.

Two things were needed:

1. **A line, not just a file, recorded per warning.** `parser::
   add_parse_warning` already tagged each warning with the file being
   parsed (`parser_source_file()`, swapped per compilation unit — unlike
   `parser_program_path()`, which stays pinned to the top-level script and
   was the wrong function one existing call site used). It now also takes
   the caller's own line number and bakes an `"\n    at FILE:LINE"` suffix
   directly into the stored message text — surviving the precompilation-
   cache round-trip for free, since `ParseEffects::warnings` only persists
   message text, not separate fields.
2. **The print site preferring the warning's own recorded location over
   `build_backtrace_string()`'s current-execution position.**
   `write_warn_to_stderr` now recognizes the `"\n    at "` suffix (the same
   way it already recognized a `"\n  in "` backtrace bake-in) and skips
   appending its own backtrace on top of it.

Every one of `add_parse_warning`'s ~10 call sites now computes its own line
via `current_line_number()` on whatever source-position slice is in scope —
straightforward for the raw-text-parsing call sites (`traits.rs`,
`param_validate.rs`, `pointy_param.rs`, `param_inner.rs`, `number.rs`,
`colonpair.rs`, `has_decl.rs`). The two AST-walk-based sink-context warning
sites (`sink_warn.rs`'s "Useless use of ... in sink context",
`simple.rs`'s xor-chain sink warning) had no raw text position available at
all — and, as a pre-existing separate bug, hardcoded a bogus `"(line 1)"`
regardless of where the warning actually fired. `sink_warn.rs`'s walk now
threads a `Cell<i64>` line tracker through its statement-list walk, updated
on every `Stmt::SetLine` marker it passes (the same bookkeeping the
compiler/runtime already rely on for line numbers); the two `simple.rs`
call sites thread the enclosing statement's own start position in.

`mutsu -I lib pcwarn.raku` now prints:

```
Potential difficulties:
    Duplicate 'is export' trait
    at lib/PrecompWarn.rakumod:2
```

matching Rakudo's file and line exactly (Rakudo additionally shows the
module name in parens and a `------>`-pointer source snippet — that
cosmetic formatting is out of scope here; no test asserts on it).

## Tests

`t/module-warning-location-misattributed.t` (new) — reuses the
`ModuleParseWarningOnceFixture` module from `t/module-parse-warning-once.t`
(duplicate `is export` on its line 5), asserting the warning names the
module's own file and line 5, not the importer's `-e line 1`, and that no
duplicate backtrace is appended on top.

PR [#6605](https://github.com/tokuhirom/mutsu/pull/6605).
