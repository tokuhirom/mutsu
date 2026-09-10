# The JIT's inline local read was 48 native instructions; now it is 25

`bench-fib`'s second-largest symbol was the JIT-generated body itself — 13.3% of
the run, reported by every profiler as a bare unnamed `???:0x…` because the code
is written into anonymous memory at runtime. [#7737](https://github.com/tokuhirom/mutsu/issues/7737)
had been open on the premise that the body got 50% slower over late August and
that the first step was to dump the generated code and diff two builds, which
nobody had done. This does that, and then fixes what the dump showed.

## Seeing inside the body

`MUTSU_JIT_DUMP` (`ops` / `clif` / `asm` / `all`) prints the accepted opcode
range, the Cranelift IR, and the backend's disassembly with every helper-shim
call address annotated by name. `MUTSU_JIT_DUMP_BIN=<dir>` also writes each
chunk's finalized bytes, and `MUTSU_JIT_DUMP_FILE` redirects the text. Crucially
every emitted instruction is tagged with the index of the opcode it came from,
so the dump carries a per-opcode byte map.

That map is what makes the body measurable. `scripts/jit-instr-profile.py`
joins a `callgrind --dump-instr=yes` run to the dumped code image — both taken
from the same invocation, since the entry address is a fresh `mmap` each run —
and prints a per-source-opcode instruction table. `docs/jit-codegen-inspection.md`
is the recipe. All of it is off unless asked for; with the variables unset the
cost is one `OnceLock` read per compiled chunk.

## What the table said

`bench-fib`, `--profile profiling`, at the merge of #7860. The fib body is 18
opcodes and runs 635,494 times, for **280 native instructions per call**:

| opcode | instr/run | share of body |
| --- | ---: | ---: |
| `GetLocal(0)` × 4 sites | **48** | **42.8%** |
| `Add` | 35 | 6.2% |
| `Sub` × 2 | 30 | 10.7% |
| `NumLe` | 22 | 7.8% |
| `Return` × 2 | 11–13 | 4.3% |
| `LoadConst` × 3 | 12 | 8.6% |
| `JumpIfFalse` | 9.5 | 3.4% |
| `CallFunc` × 2 | 7–8 | 2.7% |
| `ContainerizePair` × 2 | 4 | 1.4% |
| prologue / epilogue / regalloc moves | — | 8.7% |

One read of one scalar local cost 48 instructions, split four ways: 12 for the
spoiler latches, 8 to bounds-check the slot, **19** for the refcount-free tag
probe, and 9 to push the word.

## The 19-instruction tag probe

The probe asked four independent questions and ORed the answers: `page ==
INT_PAGE`, `NUM_PAGE_MIN <= page <= NUM_PAGE_MAX`, `word & KIND_MASK ==
BOOL_PATTERN`, `word & KIND_MASK == PACKAGE_PATTERN` — the last three needing
64-bit constants the backend had to load from the code's own pool, so three of
the 19 instructions were memory reads of literals.

The NaN-box layout already orders those cases. Pages `INT_PAGE ..= NUM_PAGE_MAX`
are *exactly* the small Int and Num words (kind words start at
`KIND_PAGE_BASE`), so the first two questions collapse into one unsigned range
test on the page, and it branches straight to the push. Above that range the
word is a kind, and `word_for_kind` is strictly monotonic in the kind id, so the
two admissible kinds — adjacent ids — collapse into a second range test,
`masked - BOOL_PATTERN <= BOOL_PACKAGE_SPAN`. Both facts are now pinned by
`const _: () = assert!` in `value::nanbox`. The accepted set is unchanged, down
to page 0 (the unused niche), which is deliberately outside the range rather
than assumed impossible: it fails the test and takes the shim.

Common path: **5 instructions instead of 19.**

## Four spoiler latches became one

The guard also loaded two process-global counters and two per-interpreter
`bool`s and ORed them — four loads, three `or`s and a test. Every one of those
inputs is monotonic and never cleared, so their OR *is* a counter that each
source bumps: `vm_jit::LOCAL_READ_SPOILERS`, now bumped by `note_container_cell`,
`note_caller_var_binding`, `mark_atomic_var_seen` and the sigilless-attribute
alias site. Folding the two per-interpreter flags in makes the latch
conservative across interpreters — one interpreter using sigilless attributes
routes every interpreter's inline read through the shim — which can only cost
speed, exactly like the process-global counters it joins. **4 instructions
instead of 12**, and `JitLayout` loses two fields.

The bounds check lost two more: `base + idx < len` is the element's own
in-bounds condition and subsumes the separate `base <= len` guard, and the sum
is the index the load needs anyway.

## Result

| | before | after |
| --- | ---: | ---: |
| `GetLocal` inline read | 48 instr | **25 instr** |
| JIT body (`bench-fib`) | 178,254,096 Ir (13.28%) | **143,302,124 Ir (10.97%)** |
| whole `bench-fib` | 1,342,290,714 Ir | **1,306,053,867 Ir (−2.70%)** |
| emitted body | 3,296 bytes | 3,160 bytes |

Pinned by `t/vm/codegen/jit-getlocal-fastpath.t`, which gains three cases for
the reduced guard: an atomic variable and a sigilless attribute must still spoil
the inline read (both had only a per-interpreter flag before), and a `Whatever`
local — one kind id below `Bool` — must still fall off the range test onto the
shim.

## And the premise: there was no codegen regression

The other half of #7737 was the claim that the generated body got 50% slower
between 2026-08-19 and 2026-08-31 while nothing in the interpreter explained it.
Building the 2026-08-20 tree with the dump facility grafted on and diffing its
`fib` body against today's answers that directly: it is the same code. The JIT
emitters (`src/vm/vm_jit_*.rs`) are byte-identical across that window apart from
two mechanical renames, and the only shape difference against today is four
instructions per `GetLocal`, from ADR-0077 Slice 2 (2026-09-08, *after* the
window) making a slot address `base + idx` into a shared stack instead of a
direct index into a per-frame `Vec`.

So the +50% was not a codegen change, and the ticket's real finding is the one
above: the generated body is expensive in absolute terms. That is worth
recording, because it is the second time an interpreter-side hypothesis about
this benchmark has been disproved by measurement rather than confirmed — #7579's
own audit closed one of its seven items as "wrong" for the same reason. The
dump and the profile script exist so the third time is cheaper.
