# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

This repo is a Rust implementation of a minimal Raku (Perl 6) compatible interpreter called **mutsu**.

## Build & run

- Build: `cargo build`
- Run: `cargo run -- <file.p6>` or `./target/debug/mutsu <file>`
- Run inline code: `cargo run -- -e 'say 42'`
- Test (cargo + prove): `make test`
- Run a single prove test: `cargo build && prove -e 'target/debug/mutsu' t/<file>.t`
- Roast (official spec tests): `make roast`
- Run a single roast test: `cargo build && prove -e 'target/debug/mutsu' roast/<path>.t`
- Pre-commit hooks (lefthook): `cargo clippy -- -D warnings` and `cargo fmt` run automatically on commit.
- Temporary test scripts: write to `tmp/` (gitignored) using the Write tool (not cat/heredoc). Build first, then run with `./target/debug/mutsu ./tmp/<file>`.

## Architecture

### Execution pipeline

Source → **Parser** (`parser_nom/`) → **Compiler** (`compiler.rs`) → **VM** (`vm.rs`) → Output

The parser is currently nom-based (`parser_nom/`) and is used unconditionally.

`parse_dispatch.rs` provides `parse_source()` used by parsing call sites and currently delegates directly to `parser_nom::parse_program()`.

Parser implementation details (dispatch order, precedence, extension checklist) are documented in `docs/parser-overview.md`.

This is a **hybrid** architecture: the bytecode VM handles primitive operations (arithmetic, comparisons, loops, jumps) natively, but delegates complex operations (user-defined function calls, method dispatch, regex, class instantiation) back to the tree-walking `Interpreter` (`runtime/`). The VM holds an embedded `Interpreter` and calls into it for anything beyond simple bytecode.

### Core data types

- **Value** (`value.rs`): Single enum with ~25 variants covering all Raku runtime types (Int, Num, Str, Bool, Rat, Complex, Array, Hash, Set, Bag, Mix, Pair, Range variants, Sub, Instance, Junction, etc.)
- **AST** (`ast.rs`): `Expr` (~50 variants) and `Stmt` (~30 variants)
- **TokenKind** (`token_kind.rs`): Shared operator/token enum used by parser/AST/compiler/runtime expression handling
- **OpCode** (`opcode.rs`): ~100 bytecode instructions, including compound loop ops

### Method dispatch (two-tier)

1. **Fast path** — `builtins/methods_0arg/`, `builtins/methods_narg.rs`: Pure Rust native methods dispatched by arity. No AST execution needed.
2. **Slow path** — `runtime/methods.rs`: Falls through from builtins for methods needing `&mut self` (say, match, map, sort with comparator, grep, new), enum dispatch, instance dispatch, and user-defined class methods.

Flow: `call_method_with_values()` tries `native_method_*arg()` first; if `None`, falls through to runtime handlers.

### Key modules

- `runtime/` (~35 submodules): Main interpreter engine — function dispatch (`calls.rs`, `dispatch.rs`), method dispatch (`methods.rs`, `methods_mut.rs`), built-in functions (`builtins_*.rs`), class system (`class.rs`), regex engine (`regex.rs`, `regex_parse.rs`), TAP test functions (`test_functions.rs`)
- `builtins/`: Pure value operations — arithmetic (`arith.rs`), native functions (`functions.rs`), native methods (`methods_0arg/`, `methods_narg.rs`), RNG (`rng.rs`), unicode (`unicode.rs`)
- `vm/`: Bytecode VM — call ops (`vm_call_ops.rs`), control flow (`vm_control_ops.rs`), data ops (`vm_data_ops.rs`), variable ops (`vm_var_ops.rs`)

### Test infrastructure

- `t/*.t`: Local tests in Raku syntax, run via prove
- `roast/`: Official Raku spec test suite (read-only git submodule)
- `roast-whitelist.txt`: Tests that pass completely; `make roast` runs only these
- `TODO_roast/`: Per-file pass/fail tracking (split by synopsis number, e.g. `TODO_roast/S02.md`)
- TAP protocol implemented in `runtime/test_functions.rs`

## Working agreements

- Perl 6 (Raku) regex is not compatible with Perl 5 regex; never assume Perl 5 compatibility.
- Prefer ASCII in source files unless a specific Unicode feature is required.
- Do not rewrite or reformat unrelated code.
- Do not use stubs, hardcoded outputs, or early returns to make tests pass.
- Commit directly to the main branch. Do not use feature branches.
- Push to remote after completing work.
- Write all documents, code comments, and commit messages in English.
- Do not use `echo`, `cat`, `printf`, or heredoc via Bash to create files. Always use the Write tool.
- Do not use `cat`, `head`, `tail`, or `sed` via Bash to read files. Always use the Read tool.
- Do not use `grep`, `rg`, or `find` via Bash to search files. Always use the Grep and Glob tools.

## Reference implementation

- `raku` is available on this system. Use `raku -e '<code>'` to check expected behavior when the spec is unclear.
- When investigating a roast test, always run it with `raku` first to see the expected output before comparing with mutsu.
- Design docs: `./old-design-docs/`
- Raku language documentation: `./raku-doc/` — consult these docs when the language spec or behavior is unclear.

## Roast (official Raku test suite)

- The ultimate goal is to pass ALL roast tests. Pick any failing test and work on it.
- `roast/` is read-only; never modify files under `roast/`.
- `TODO_roast/` tracks per-file pass/fail status (split by synopsis number). Mark a test `[x]` only when **all** subtests pass.
- When a test file has known partial failures, add indented notes under its entry describing the blockers.
- Do not add a roast test to the whitelist unless `prove -e 'cargo run --' <file>` exits cleanly.
- Never add special-case logic, hardcoded results, or test-specific hacks just to pass a roast test. Every fix must be a genuine, general-purpose improvement.
- When the expected behavior is unclear, consult `./old-design-docs/` for the original Raku language specification.
- When investigating a roast test and deciding to defer it, always record the reason for failure in the relevant file under `TODO_roast/`.
- Roast is the authoritative spec. If passing a roast test requires changing a local test under `t/`, update the local test.
- When `make roast` shows failures in whitelisted tests, investigate each failure — do NOT dismiss them as "pre-existing".
- Never remove a previously passing test from the whitelist due to a regression. If a change causes a whitelisted test to fail, fix the regression so the test passes again.
- When a roast test requires solving multiple unrelated prerequisites, fix what you can, update the relevant file under `TODO_roast/`, and move on.

## Working on complex features

- Do NOT fear complex cases. Every feature must eventually be implemented fully.
- Deferring hard work does not make it go away — tackle difficult features head-on.
- When a roast test requires a complex feature (e.g., attribute traits, arbitrary regex delimiters, module precompilation), implement it rather than skipping to easier tests.

## Roast test prioritization

- Run `./scripts/roast-history.sh` to generate per-file category lists under `tmp/`:
  - `tmp/roast-panic.txt` — Rust panics (highest priority)
  - `tmp/roast-timeout.txt` — timeouts
  - `tmp/roast-error.txt` — no valid TAP plan
  - `tmp/roast-fail.txt` — some subtests failing
  - `tmp/roast-pass.txt` — fully passing
- Priority order: panic → timeout → error/fail.
- Do NOT cherry-pick "almost passing" tests. Pick tests randomly from the fail/error lists. The goal is broad language coverage, not gaming the pass count.
- After making changes, always run `./scripts/roast-history.sh` to regenerate category lists and check for newly passing tests. Compare the new `tmp/roast-pass.txt` against the whitelist to find tests to add.

## Checking `make roast` results

```
make roast 2>&1 | grep -E "(FAILED|Failed)" | head -20
make roast 2>&1 | grep -E "(not ok|FAILED|Failed|Wstat)" | head -20
```

## Parallel agents with git worktrees

- Each sub-agent MUST work in its own git worktree to avoid file conflicts.
- Create worktrees under `.git/worktrees-work/` (not `/tmp`):
  ```
  git worktree add .git/worktrees-work/<name> -b worktree/<name> HEAD
  ```
- Sub-agents work, build, and test within their worktree. They do NOT commit or push.
- Integrate immediately when a sub-agent finishes; launch a new agent to keep the pipeline full.
- To integrate: export diff from worktree, apply to main repo, fix clippy/fmt, run `make roast`, commit and push.
- Clean up after integration:
  ```
  git worktree remove .git/worktrees-work/<name>
  git branch -d worktree/<name>
  ```
- Always ensure cwd is the main repo before running `make roast`, `git commit`, or `git push`.
- Prefer manual edits over `git apply` since the worktree base commit differs from main HEAD after incremental integration.

## Debugging guidelines

- Do NOT use printf debugging (eprintln! → build → check → repeat). Rust builds are slow.
- Preferred approaches in order:
  1. **AST dump**: `./target/debug/mutsu --dump-ast <file>` or `--dump-ast -e '<code>'`
  2. **Trace logs**: `MUTSU_TRACE=1 ./target/debug/mutsu <file>` (filter: `MUTSU_TRACE=eval` or `MUTSU_TRACE=parse,vm`)
  3. **Focused unit tests**: `#[test]` in the relevant module, run with `cargo test <name>`
  4. **Read the code**: Trace logic by reading, not running
  5. **Debugger**: `rust-gdb ./target/debug/mutsu`
- If you must add debug prints, add ALL of them in one pass. Always remove before committing.

## Raku's context-dependent parsing (slangs)

Raku's grammar is not a single monolithic grammar — it switches between sub-languages ("slangs") depending on context:
- **Main** slang: statements, expressions, operators
- **Regex** slang: inside `/ /`, `rx/ /`, `m/ /`, grammar tokens/rules
- **Quote** slang: inside `" "`, `' '`, `q/ /`, `qq/ /`, heredocs
- **Pod** slang: documentation blocks (`=begin`, `=for`, `=head`, etc.)

Each slang has its own grammar rules (e.g., `+` means repetition in Regex slang but addition in Main slang). Raku's official grammar (`Raku::Grammar`) handles this via slang switching at parse time.

**Implication for mutsu**: nom is a single-grammar parser combinator framework and does not natively support slang switching. As we add `grammar`/`token`/`rule` support (which let users define custom grammars), we may need an architecture that can switch parsing modes contextually. Keep the nom parser modular so that individual sub-parsers (regex, quote, etc.) can be extracted and reused in a future architecture.

## Conventions

- Keep each Rust source file under 500 lines. Split into smaller modules if exceeded.
- Write feature tests using prove (`t/*.t`).
- Use Rust unit tests (`#[test]`) for internal components like parser and runtime helpers.
- Every feature addition must include tests.
