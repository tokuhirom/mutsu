# Reading what the JIT actually emitted

The Tier A/B JIT (ADR-0004) writes its bodies into anonymous memory at runtime,
so nothing on disk carries a symbol for them. A profiler therefore reports the
generated code as a single unnamed `???:0x…` entry — on `bench-fib` that entry
was **13.3% of the whole run**, the second-largest symbol in the benchmark, with
no way to see inside it
([#7737](https://github.com/tokuhirom/mutsu/issues/7737)). Two rounds of
guessing at its cost from the interpreter side produced nothing; both this
document and the tooling it describes exist so the next session measures
instead.

Everything here is off unless an environment variable asks for it. With the
variables unset the only cost is one `OnceLock` read per compiled chunk.

## `MUTSU_JIT_DUMP` — the emitted code

| value | what you get |
| --- | --- |
| `ops` | the accepted opcode range, the chunk's locals, the emitted code size and the finalized entry address |
| `clif` | the above plus the Cranelift IR handed to the backend |
| `asm` | the above plus the backend's disassembly, with each helper-shim call address annotated by name (`; -> call_func`) |
| `all` / `1` | `clif` and `asm` together |

Two companions:

- `MUTSU_JIT_DUMP_FILE=<path>` — append the dump there instead of stderr.
- `MUTSU_JIT_DUMP_BIN=<dir>` — also write each chunk's finalized bytes to
  `<dir>/<name>.bin`, so the body can be disassembled at its real load address
  and lined up with a profile.

Pair it with `MUTSU_JIT_THRESHOLD=2` to make a small script compile its hot
chunk immediately:

```
cargo build
MUTSU_JIT_THRESHOLD=2 MUTSU_JIT_DUMP=asm MUTSU_JIT_DUMP_FILE=tmp/jit.txt \
  ./target/debug/mutsu tmp/fib.raku
```

The Rust optimization level does **not** affect what the JIT emits — Cranelift
compiles the same bytecode either way — so a `cargo build` (debug) dump is
byte-for-byte the dump a release binary would produce, modulo helper addresses
and `Interpreter` field displacements. Use the debug build to read codegen; the
`profiling` profile is only needed when the *profile* must include symbolized
interpreter frames.

### Per-opcode byte spans

When a dump is on, every emitted instruction is tagged with the index of the
opcode it came from, and the `asm` dump prints the resulting map:

```
  span 0x0021..0x005e op 0
  span 0x0066..0x0084 op 0
  ...
  span 0x0154..0x0175 op 1
```

`op 4294967295` is code with no source opcode: the prologue, the epilogue and
the register moves the register allocator inserted between opcodes. The spans
are what turn a flat profile of the body into a per-opcode cost table.

## Per-opcode instruction profile

`perf` cannot help here (and is absent from remote containers entirely), but
`callgrind` counts retired instructions deterministically and
layout-insensitively — the right oracle at this effect size, per
[#7579](https://github.com/tokuhirom/mutsu/issues/7579)'s method notes.

```
cargo build --profile profiling
MUTSU_JIT_DUMP=asm MUTSU_JIT_DUMP_FILE=tmp/jit.txt MUTSU_JIT_DUMP_BIN=tmp/jitbin \
  valgrind --tool=callgrind --dump-instr=yes \
           --callgrind-out-file=tmp/cg.out \
           ./target/profiling/mutsu benchmarks/bench-fib.raku
scripts/jit-instr-profile.py tmp/cg.out tmp/jit.txt tmp/jitbin
```

`--dump-instr=yes` is what makes callgrind record per-address counts; the script
joins those to the disassembly through the entry address and spans the dump
printed in the same run (the address is a fresh `mmap` each run, so both halves
must come from one invocation). Output:

```
=== mutsu_jit_1  0x4847000..0x4847ce0 (3296 bytes)  self Ir=178254096 (13.28% of run) ===

op               Ir   share     runs instr/run  opcode
0          30503712  17.11%   635494      48.0  GetLocal(0)
4          15252288   8.56%   317756      48.0  GetLocal(0)
2          13980868   7.84%   635494      22.0  NumLe
16         11120830   6.24%   317738      35.0  Add
...
glue       15569451   8.73%                     prologue / regalloc moves
```

followed by the whole body annotated instruction-by-instruction with its Ir
count and owning opcode. `instr/run` is the number that matters: it says what
one execution of that opcode costs in native instructions, independent of how
often the benchmark runs it.

## Comparing two revisions

To answer "did codegen change between commit A and commit B", build both and
diff the dumps rather than bisecting — per-commit code-layout noise is ~5% and
swamps the effects being chased ([#7579](https://github.com/tokuhirom/mutsu/issues/7579)).

```
git worktree add /tmp/old <commit>          # outside the repo, or add `[workspace]`
                                            # to the worktree's Cargo.toml
# copy src/vm/vm_jit_dump.rs and its two call sites in if the old tree predates them
cargo build && MUTSU_JIT_THRESHOLD=2 MUTSU_JIT_DUMP=asm ... > old.txt
```

Normalize before diffing: helper-shim addresses, `Interpreter` field
displacements and Cranelift's block numbering all move for reasons unrelated to
the shape of the code.

That comparison is what settled #7737: the 2026-08-20 and 2026-09-10 bodies for
`fib` are the same code, four instructions per `GetLocal` apart (ADR-0077 Slice
2 made a slot address `base + idx` in a shared stack instead of a direct index
into a per-frame `Vec`). There was no codegen regression to find — the body is
simply expensive in absolute terms, which is a different problem with a
different fix.
