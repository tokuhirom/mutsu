# `--dump-ast` and the analysis API no longer run `use`d modules

`--dump-ast`, `--dump-bytecode` and the analysis API behind the language server
(`analysis::check` / `analysis::symbols`, ADR-0065 D4) promise not to execute the
program, but two parse-time probes still ran a `use`d module's mainline: the
dynamic `EXPORT::*` probe that learns computed export names (#9500) and slang
activation (ADR-0026). A module with side effects — writing a file, say — did them
while its importer was merely being dumped or opened in an editor.

The parser now has a non-executing mode (`parser::no_execute`), held by a guard
those entry points take. Under it both probes are skipped: computed export
names fall back to what the static export scan found, and a slang-activating
`use` leaves the unit in the ordinary grammar. Running the program is unchanged
(#11212).
