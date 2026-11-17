# A `use` loads its module when the parser reaches it

ADR-17558 slice 1. The parser now asks the interpreter that compiles a unit to
load a `use`d module as soon as it has parsed the `use` statement, as rakudo
does at BEGIN time. The module's mainline runs then, once; the unit's run-time
`use` finds it loaded and only imports.

The seam is a leaf trait, `compile_time_loader::CompileTimeLoader`, that the
parser names without naming the runtime. An interpreter pushes itself as the
loader for the extent of a parse whose unit it will run (a program, `-c`, an
`EVAL`); a parse with no loader pushed
loads nothing, so `--dump-*`, the analysis API and the language server stay
non-executing by construction. While a module compiles in the middle of the
importer's parse, the importer's per-unit parser state is suspended
(`parser::unit_state`) and restored afterwards.

Visible differences, each now rakudo's behaviour:

- A syntax error after a `use` is reported after the module has run.
- A module that dies while loading fails the importer's compile at the `use`:
  nothing of the importer's mainline runs.

Not yet moved to parse time (later slices of the ADR): the `use`s of a module's
own compile and of a `require`d file (they load in its prologue, as on a
precompilation hit), a `use` after a `BEGIN`
or a `multi`/`proto` of the same unit (those effects still happen after the
parse), a module that imports through a `sub EXPORT` hook, a `use` with
arguments or `:if`, and the slang activation interpreter's own loads.
