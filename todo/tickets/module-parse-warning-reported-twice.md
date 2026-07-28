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
