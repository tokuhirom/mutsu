# A compiled chunk knows which file its lines belong to

`CompiledCode` has carried a static ip -> line table (`op_lines`) since the
`SetSourceLine` opcode was retired, so a chunk could answer "instruction 412 is
line 89" — but not "…of which file". The file was recoverable only at runtime,
and from three unrelated places: a `?FILE` env probe
(`Interpreter::current_source_file`), the per-frame `RoutineFrame { file,
def_file }`, and `CompiledFunction::source_file`. Three runtime sources for a
fact that is known at compile time, and not one of them addressable from
`(chunk, ip)` alone.

This is ADR-0106 Slice 0, and the profiler that ADR describes is not the only
thing that wanted it: error metadata and backtraces reach for an env probe to
answer a question the chunk already knew the answer to, and anything else that
wants to point at source from bytecode — a `--dump-bytecode` annotation, the
language-server surface of ADR-0065 — needs the same thing.

## What changed

`CompiledCode` gained a `source_file: Option<Symbol>`, and `location_at(ip)`
beside `line_at(ip)` returns the full `(file, line)` for an instruction.
`line_at`'s contract is untouched: 0 still means "leave the current line alone",
and `location_at` never reports line 0 either.

The interesting part is how a chunk gets the field. Threading a path into
`Compiler::compile` was the obvious route and the wrong one: chunk compilers are
constructed in about forty places (sub bodies, method bodies, declaration-
expression thunks, the hoist pass), and a field threaded through forty
constructors is forty chances to forget one. Instead the unit's identity is
*published* for the duration of a compile, through a thread-local that
`CompiledCode::new()` reads — the same shape the parser already uses for `$?FILE`
(`parser::set_parser_source_file`). Every chunk a compile produces is stamped,
including the nested ones no walker enumerates. The new
`src/unit_source_file.rs` holds the thread-local and its RAII guard; a `None`
inherits the enclosing unit rather than clearing it, so a compile that cannot
name its own file still lands under whatever is running.

Four places publish a unit:

- `Interpreter::run` publishes the script for the whole run — the BEGIN-time
  compiles, the mainline, and every on-the-fly compile the running program
  triggers.
- a module load publishes the module's own path, from just after its source is
  parsed until the load returns, so a `use`d module's routines are not
  attributed to the script that used them.
- `compile_block_raw` and `compile_block_value_opts` publish `?FILE`, which is
  what an on-the-fly recompile belongs to.
- an `EVAL` needs nothing new: `builtin_eval` already scopes `?FILE` to the
  unit's synthesized name (`EVAL_<N>`, what `Code.file` and a backtrace report),
  so the chunk picks that up through `compile_block_value_opts`. Minting a
  second identity for EVAL units was the first attempt and was dropped — a
  location that disagrees with the backtrace beside it is worse than no
  location.

`CompiledCode::stamp_source_file` covers the one case the ambient guard cannot:
a nested named sub is compiled as part of its enclosing routine, before that
routine's file is known. `CompiledFunction::stamp_source_file` — which already
walked exactly that tree for the routine-level `source_file` — now stamps the
bytecode half on the way through.

`--dump-bytecode` is the first consumer: each instruction whose location differs
from the one before it is annotated with `; file:line`.

## Tests

`src/unit_source_file.rs` carries the pins: the guard's nesting and inheritance,
that a guarded compile stamps *every* chunk it produces (with `location_at`
agreeing with `line_at` at every ip), and an end-to-end fixture — a script that
`use`s a module and runs an `EVAL` — where each of the three units is named by
the chunks that came from it, the EVAL'd closure's chunk included.

Closes #8699. ADR-0106 Slices 1-5 (the `gc_safepoint` poll network, the sampler,
exact per-line counts, region tags, the report schema) are unaffected and still
open.
