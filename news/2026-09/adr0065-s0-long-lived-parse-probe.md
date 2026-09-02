# The language server's viability gate passes: mutsu holds up as a resident parser

ADR-0065 designed a language server for mutsu and put a gate in front of everything else.
mutsu has always been a one-shot process — parse once, run, exit — and a language server
inverts that: one process parses many documents and re-parses each of them thousands of
times. D8 made that an explicit precondition: *measure it before writing a server
skeleton, because if it fails the rest of the plan is invalid.*

S0 is now done. The probe is `tests/long_lived_parse.rs` — five tests that run under
`cargo test` and stay in the suite as regression gates, with the iteration count behind
`MUTSU_S0_ITERATIONS` so the committed gate is cheap and a deeper sweep is one env var
away. **The gate passes**, and two of D8's own expectations turned out to be wrong.

## Byte-identical re-parse is the wrong property to want

The first version of the probe asserted that re-parsing an unchanged document yields an
identical AST. It failed immediately, on three counts: `__with_tmp_N`, `__ANON_CLASS_N__`,
and `decl_id`.

None of those is a defect. `decl_id` is a `my class`'s key in the global type registry —
ADR-0047 D1 mangles every lexical declaration to `Foo\u{0}<decl-id>` *precisely* so two
declaration sites can never share a key. `__ANON_CLASS_N__` and `__ANON_ROLE_N__` are
registry names, drawn from the same counters the runtime's `but`-mixin path draws from.
Resetting any of them per parse would make two sites in two different compilation units
collide in a process-global table.

So the gate is determinism *modulo ids the parser is required to mint uniquely per site*.
The probe normalizes those and compares everything else; 8000 consecutive re-parses are
identical, and any other difference fails the test as residual parser state. Aiming at
byte-identity would have led to exactly the wrong fix.

## The only unbounded growth is one leaked name per anonymous declaration per parse

`src/symbol.rs` leaks interned strings for the process lifetime by design — free for a
one-shot process, a slow leak for a resident one. Splitting the measurement by whether the
document contains an anonymous declaration isolated it completely (8000 re-parses, debug
build):

| Document | Interned names | Resident memory |
| --- | --- | --- |
| No anonymous declarations | **+0** | +124 KiB at 8000 parses, +136 KiB at 2000 — noise, not growth |
| One anonymous class | +8000 (exactly 1.00/parse) | +3988 KiB (~0.5 KiB/parse, linear) |

There is no general per-parse leak. The memo tables reset and genuinely release; `Vec` and
`String` churn returns to the allocator; a document with no anonymous declaration reaches
*exactly zero* interning growth, which is now the tighter of the two committed gates. The
entire linear component is the freshly minted registry name for each anonymous
declaration.

The fix is structural and belongs with the server's real entry point rather than here: an
analysis-only parse never registers a type, so in that mode the uniqueness requirement
drops from process-global to compilation-unit-local. Recorded as
`todo/tickets/analysis-parse-mints-process-unique-registry-names.md`, to be done with the
S1 parse API instead of retrofitted onto `dump_ast`.

## Concurrent parsing holds, which D8 did not expect

D8 predicted that concurrency across documents "almost certainly will not" hold and that
the server would have to serialize parsing. It does hold. The parser's entire working set
is thread-local — `SCOPES`, the three memo tables, `ORIGINAL_SOURCE`, `LEAKED_REGIONS`,
the slang modes — the symbol table sits behind an `RwLock`, and the unique-id counters are
atomics. Four threads parsing four different documents, five rounds, produce ASTs identical
to the same documents parsed on the main thread. That is now pinned, so a future change
that promotes parser state to a process-global fails in `cargo test` rather than in a
server under load. It is scoped to *parsing*: it says nothing about concurrently loading
modules or executing, which touch the type registry and the interpreter's globals.

## No residue between documents, and line numbers survive re-entry

The failure mode a one-shot process can never expose is document A changing how document B
parses afterwards — custom operators, a `use v6.e.PREVIEW` pragma, lexicals and a
`constant` all live in thread-local parser state. Since there is no pristine process to
compare against inside one test, the probe uses a B/A/B sandwich (and an A/B/A one, so the
check cannot be satisfied by B failing identically twice). No residue.

`ORIGINAL_SOURCE`, the thread-local `(raw pointer, length)` pair behind `$?LINE`, also
survives being interleaved across differently sized buffers, including buffers that trigger
nested sub-parses on separate allocations (a heredoc and an `EVAL`). The small document's
line numbers stay correct after 25 interleaved rounds, so `parse_program_partial`'s
snapshot/restore discipline holds under repetition. Since `Stmt::SetLine` is the only
positional information mutsu has (ADR-0065 D6), that is the load-bearing property under
every diagnostic the server will emit.

One measurement came out better than the design assumed. ADR-0065's feasibility table put
a module parse at ~9 ms, measured through the CLI — one process per file, startup and I/O
included. In process, which is what a server does, a release build re-parses the probe
document in **1.29 ms**. D3 dropped incremental document sync on the argument that a full
reparse is fast enough; the real figure is about seven times better than the number that
argument rested on.

`src/symbol.rs` gained one public function for this, `symbol::interned_count()` — the table
is append-only and leaked, so its size doubles as the leak gauge the probe reads.
