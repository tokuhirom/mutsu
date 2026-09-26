# mutsu architecture overview

A map of the interpreter for orientation. The *rules* that govern changes to it (no new slow-path
fallbacks, one implementation per primitive, `// Cost:` annotations, ...) live in
[`AGENTS.md`](../AGENTS.md); this file is the reference material behind them. Parser internals are
in [`parser-overview.md`](parser-overview.md), and the decisions that shaped each subsystem are the
ADRs under [`adr/`](adr/).

## Execution pipeline

Source → **Parser** (`parser/`) → **Compiler** (`compiler/`) → **VM** (`vm/`) → Output

The parser is in `src/parser/` and is used unconditionally.

`parse_dispatch.rs` provides `parse_source()` used by parsing call sites and currently delegates directly to `parser::parse_program()`.

Parser implementation details (dispatch order, precedence, extension checklist) are documented in [`parser-overview.md`](parser-overview.md).

This is a **bytecode VM** architecture. The VM handles ALL operations natively via compiled bytecode. The standalone tree-walking interpreter has been eliminated (CP-1/CP-2/CP-3, #3075–#3104): there is now a single `struct Interpreter` that *is* the bytecode VM. `eval_block_value()` survives only as a carrier for re-entrant source evaluation (`EVAL`, embedded `{...}` blocks in regexes), delegating to the same native implementations — it is not a separate execution engine. The `runtime/methods.rs` slow path (see below) and the `env_dirty` dual store are the remaining tree-walk-era mechanisms still being paid down.

**IMPORTANT: Do NOT add new slow-path / tree-walk fallbacks.** When implementing a new feature:
- Implement it in the compiler (`compiler/`) to emit bytecode, and in the VM (`vm/`) to execute it.
- Do NOT route new features through `call_method_with_values()` → `run_instance_method()` or the other `runtime/methods.rs` slow-path handlers.
- Existing slow-path fallbacks are technical debt to be eliminated, not a pattern to follow.
- If you must use the slow path as a temporary measure, leave a `// TODO: compile to bytecode` comment.

## Core data types

- **Value** (`value/`): Single enum with ~25 variants covering all Raku runtime types (Int, Num, Str, Bool, Rat, Complex, Array, Hash, Set, Bag, Mix, Pair, Range variants, Sub, Instance, Junction, etc.)
- **AST** (`ast.rs`): `Expr` (~50 variants) and `Stmt` (~30 variants)
- **TokenKind** (`token_kind.rs`): Shared operator/token enum used by parser/AST/compiler/runtime expression handling
- **RuntimeError** (`value.rs`): Runtime/parse error carrier; parse failures now use structured metadata (`code`, `line`, `column`) in addition to `message`
- **OpCode** (`opcode.rs`): ~375 bytecode instructions, including compound loop ops. Keep `size_of::<OpCode>()` <= 48 bytes (pinned by the `opcode_size_guard` test) — box any fat variant payload (see `docs/opcode-design-review.md`). Every `OpCode::` arm in `exec_one_dispatch` carries a `// Cost:` line (see "Conventions" in `AGENTS.md`)

## Method dispatch (two-tier)

1. **Fast path** — `builtins/methods_0arg/`, `builtins/methods_narg.rs`: Pure Rust native methods dispatched by arity. No AST execution needed.
2. **Slow path** — `runtime/methods.rs`: Falls through from builtins for methods needing `&mut self` (say, match, map, sort with comparator, grep, new), enum dispatch, instance dispatch, and user-defined class methods.

Flow: `call_method_with_values()` tries `native_method_*arg()` first; if `None`, falls through to runtime handlers.

## Compiler (`src/compiler/`)

Compiles the AST into bytecode (`OpCode` instructions). `mod.rs` is the entry point (`compile()`,
the `Compiler` struct); statement and expression compilation (`compile_stmt()` / `compile_expr()`)
fan out over ~40 files, with the constant pool, local-slot allocation and operator mapping in the
helpers.

## VM (`src/vm.rs`, `src/vm/`)

Executes compiled bytecode. `src/vm.rs` declares the submodules; the unified `Interpreter`
(`src/interpreter.rs`) *is* the VM. The opcode dispatch match lives in `vm/vm_exec_dispatch.rs`
(`exec_one_dispatch`, every arm carrying a `// Cost:` line), and each arm delegates to an
`exec_*_op` handler in a family file. There are ~170 files; the prefix names the family:

- `vm_arith_*`, `vm_bitwise_ops`, `vm_comparison_*`, `vm_set_*`: arithmetic, comparison, set ops
- `vm_call_*`, `vm_method_dispatch`, `vm_native_*`: sub/method calls and native fast lanes
- `vm_var_*`: variable get/assign, indexing, element mutation, typed containers
- `vm_for_loop_*`, `vm_loop_*`, `vm_control_ops`, `vm_given_when_ops`, `vm_try_catch_ops`: control flow
- `vm_register_*`, `vm_typedecl_ops`, `vm_module_ops`: sub/class/role/enum registration, module loading
- `vm_subst_*`, `vm_string_regex_ops`, `vm_smartmatch_*`: substitution, regex, smartmatch
- `vm_misc_*`: ranges, coercion, reductions, scopes, type checks
- `vm_react_*`, `vm_hyper_*`: supplies/react and hyper/race
- `vm_jit*`: the JIT tiers
- `vm_helpers*`, `vm_*_helpers`: shared helpers (env lookup, junction threading, lazy adaptors)

## Other key modules

- `runtime/` (~370 files): dispatch (`calls*`, `dispatch*`), the method slow path (`methods*`),
  built-in routines (`builtins_*`), the class system, the regex engine.
- `builtins/`: pure value operations — the shared arithmetic and string primitives (`arith/`,
  `str_prim/`), native functions, the native methods (`methods_0arg/`, `methods_narg/`), RNG and
  the Unicode tables.
- `value/`: the NaN-boxed `Value` and its bodies (strings, seqs, hashes, native backing).

## Test infrastructure

- `t/<category>/**/*.t`: Local tests in Raku syntax, run via prove. `t/` is a **nested tree, not a flat directory** — a new test goes in one of the sixteen categories, and **[t-directory-layout.md](t-directory-layout.md) is the authority on which one**. Read it before adding a file. In short: place by what the test would catch if it broke (not by the syntax it uses), keep basenames globally unique, never nest more than two levels below `t/`, and never put a `.t` at `t/` top level or under `t/lib` / `t/fixtures` / `t/packages`. `make check-t-layout` (also a `make test` prerequisite and a CI step) enforces all of that.
- Every invocation of the suite passes `prove -r` — prove does not descend into subdirectories without it.
- `roast/`: Official Raku spec test suite (vendored, read-only)
- `roast-whitelist.txt`: Tests that pass completely; `make roast` runs only these
- `TODO_roast/BLOCKERS.md`: the single ledger of all non-whitelisted roast tests, tracked per file and by root cause, with a raku-baseline column. Use this to decide which feature to implement next for maximum roast progress. (The per-synopsis `TODO_roast/S*.md` checklists were retired 2026-07-15 and merged into it.)
- `TODO_roast/raku-baseline.md` / `.tsv`: the roast × raku reference-run baseline (generated by `scripts/roast-raku-baseline.sh`).
- `Test` is the real upstream module run verbatim (the native provider was deleted on 2026-09-10), so TAP output comes from Raku code, not from `src/`.

## Parser error metadata

- `parser::parse_program()` maps parse failures to `RuntimeError` with structured metadata:
  - `code`: `RuntimeErrorCode::{ParseUnparsed, ParseExpected, ParseGeneric}`
  - `line`, `column`: 1-based source location where available
- CLI output (`main.rs`) prints this metadata as a separate line (`metadata: code=..., line=..., column=...`) to make failures easier to inspect and machine-process.

## Raku's context-dependent parsing (slangs)

Raku's grammar is not a single monolithic grammar — it switches between sub-languages ("slangs") depending on context:
- **Main** slang: statements, expressions, operators
- **Regex** slang: inside `/ /`, `rx/ /`, `m/ /`, grammar tokens/rules
- **Quote** slang: inside `" "`, `' '`, `q/ /`, `qq/ /`, heredocs
- **Pod** slang: documentation blocks (`=begin`, `=for`, `=head`, etc.)

Each slang has its own grammar rules (e.g., `+` means repetition in Regex slang but addition in Main slang). Raku's official grammar (`Raku::Grammar`) handles this via slang switching at parse time.

**Implication for mutsu**: The parser does not natively support slang switching. As we add `grammar`/`token`/`rule` support (which let users define custom grammars), we may need an architecture that can switch parsing modes contextually. Keep the parser modular so that individual sub-parsers (regex, quote, etc.) can be extracted and reused in a future architecture.

## GC / JIT status (ADR-0001 — SHIPPED; read the ADR before touching GC/JIT internals)

The strategy was fixed in [docs/adr/0001-gc-strategy-and-phasing.md](adr/0001-gc-strategy-and-phasing.md) and **has been executed**: ADR-0001 §7 (2026-08-02) records layers 3a (Bacon-Rajan cycle collector on the container-kind `Gc<T>` variants, scalar variants GC-free), 3b (NaN-boxing), and 4 (JIT) as **all shipped and default on** (GC's default-on trigger was decided by [ADR-0003](adr/0003-default-on-gc-trigger.md); CI exercises them via the `gc-stress` and `jit-stress` jobs). What agents need to know now:

- **Do NOT treat GC/JIT as pending work.** mutsu collects cycles; JIT is the default configuration (the bench history's plain rows pin `MUTSU_JIT=off` as the interpreter baseline).
- **The old "Track B is fused with GC, do not start it standalone" rule is superseded** by [ADR-0013](adr/0013-container-interior-mutability-cellvalue.md) §7: the `GcBox`/`UnsafeCell` interior-mutability refinement made the `gc_contents_mut` sites sound at the primitive, with no Value-layer element-cell migration. Work that ADR-0001 once deferred onto "the GC campaign" (e.g. store-side element itemization, `news/2026-09/element-itemization-lost-in-scalar-binding.md`) is now unblocked and stands on its own.
- **Still open from ADR-0001:** §4.3 (Phase A' root-consolidation scope) and layer 3c (biased reference counting — a perf option, not a prerequisite for anything).
- **Level-2 (full VM redesign for MoarVM-style precise moving/tracing GC) remains rejected** unless level-1 JIT hits a *measured* refcount ceiling — do NOT start it without a new/updated ADR.
