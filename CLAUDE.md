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
- Module search paths: use `-I <path>` to add a module search path, or set the `MUTSULIB` environment variable (colon-separated paths). `-I` paths take priority over `MUTSULIB` paths.
  - Example: `cargo run -- -I lib script.raku`
  - Example: `MUTSULIB=/path/to/lib1:/path/to/lib2 cargo run -- script.raku`
- Help: `cargo run -- --help`
- Environment variables: secrets (e.g. `GH_TOKEN`) are stored in `.env` and loaded via `dotenvx run --`. Use `dotenvx run -- <command>` to run commands that need these secrets (e.g. `dotenvx run -- bash scripts/ai-supervisor.sh --list`).

## Architecture

### Execution pipeline

Source → **Parser** (`parser/`) → **Compiler** (`compiler/`) → **VM** (`vm/`) → Output

The parser is in `src/parser/` and is used unconditionally.

`parse_dispatch.rs` provides `parse_source()` used by parsing call sites and currently delegates directly to `parser::parse_program()`.

Parser implementation details (dispatch order, precedence, extension checklist) are documented in `docs/parser-overview.md`.

This is a **hybrid** architecture: the bytecode VM handles primitive operations (arithmetic, comparisons, loops, jumps) natively, but delegates complex operations (user-defined function calls, method dispatch, regex, class instantiation) back to the tree-walking `Interpreter` (`runtime/`). The VM holds an embedded `Interpreter` and calls into it for anything beyond simple bytecode.

### Core data types

- **Value** (`value.rs`): Single enum with ~25 variants covering all Raku runtime types (Int, Num, Str, Bool, Rat, Complex, Array, Hash, Set, Bag, Mix, Pair, Range variants, Sub, Instance, Junction, etc.)
- **AST** (`ast.rs`): `Expr` (~50 variants) and `Stmt` (~30 variants)
- **TokenKind** (`token_kind.rs`): Shared operator/token enum used by parser/AST/compiler/runtime expression handling
- **RuntimeError** (`value.rs`): Runtime/parse error carrier; parse failures now use structured metadata (`code`, `line`, `column`) in addition to `message`
- **OpCode** (`opcode.rs`): ~100 bytecode instructions, including compound loop ops

### Method dispatch (two-tier)

1. **Fast path** — `builtins/methods_0arg/`, `builtins/methods_narg.rs`: Pure Rust native methods dispatched by arity. No AST execution needed.
2. **Slow path** — `runtime/methods.rs`: Falls through from builtins for methods needing `&mut self` (say, match, map, sort with comparator, grep, new), enum dispatch, instance dispatch, and user-defined class methods.

Flow: `call_method_with_values()` tries `native_method_*arg()` first; if `None`, falls through to runtime handlers.

### Compiler (`src/compiler/`)

Compiles AST into bytecode (`OpCode` instructions):

- `mod.rs`: Entry point — `compile()` function, `Compiler` struct
- `stmt.rs`: Statement compilation (`compile_stmt()`)
- `expr.rs`: Expression compilation (`compile_expr()`)
- `helpers.rs`: Shared helpers — constant pool management, local variable slots, operator mapping

### VM (`src/vm/`)

Executes compiled bytecode. `vm.rs` contains the VM struct, `run()`, and a thin `exec_one()` dispatch match that delegates to submodules:

- `vm_arith_ops.rs`: Arithmetic, logic, bitwise, repetition, mixin, pair ops
- `vm_comparison_ops.rs`: Numeric/string comparison, three-way, identity, divisibility
- `vm_set_ops.rs`: Set/Bag/Mix operations, junctions
- `vm_call_ops.rs`: Function calls, method calls, hyper method calls
- `vm_control_ops.rs`: Loops (while, for, C-style, repeat), given/when, try/catch
- `vm_data_ops.rs`: Array/hash construction, say/print/note I/O
- `vm_var_ops.rs`: Variable access (global, array, hash, bare word), indexing, post-increment/decrement
- `vm_register_ops.rs`: Closures, sub/class/role/enum registration, module loading
- `vm_string_regex_ops.rs`: Substitution, transliteration, hyper/meta/infix ops
- `vm_misc_ops.rs`: Range creation, coercion, reduction, magic vars, block scope, let
- `vm_helpers.rs`: Shared helpers — env lookup, type checking, junction threading, compiled function dispatch

### Other key modules

- `runtime/` (~35 submodules): Main interpreter engine — function dispatch (`calls.rs`, `dispatch.rs`), method dispatch (`methods.rs`, `methods_mut.rs`), built-in functions (`builtins_*.rs`), class system (`class.rs`), regex engine (`regex.rs`, `regex_parse.rs`), TAP test functions (`test_functions.rs`)
- `builtins/`: Pure value operations — arithmetic (`arith.rs`), native functions (`functions.rs`), native methods (`methods_0arg/`, `methods_narg.rs`), RNG (`rng.rs`), unicode (`unicode.rs`)

### Test infrastructure

- `t/*.t`: Local tests in Raku syntax, run via prove
- `roast/`: Official Raku spec test suite (vendored, read-only)
- `roast-whitelist.txt`: Tests that pass completely; `make roast` runs only these
- `TODO_roast/`: Per-file pass/fail tracking (split by synopsis number, e.g. `TODO_roast/S02.md`)
- TAP protocol implemented in `runtime/test_functions.rs`

### Parser error metadata

- `parser::parse_program()` maps parse failures to `RuntimeError` with structured metadata:
  - `code`: `RuntimeErrorCode::{ParseUnparsed, ParseExpected, ParseGeneric}`
  - `line`, `column`: 1-based source location where available
- CLI output (`main.rs`) prints this metadata as a separate line (`metadata: code=..., line=..., column=...`) to make failures easier to inspect and machine-process.

## Working agreements

- **Do NOT confuse `Test::Util` functions with builtins.** Functions like `is_run`, `doesn't-hang`, `make-temp-dir`, `make-temp-file` come from `roast/packages/Test-Helpers/lib/Test/Util.rakumod`, NOT from the Raku core. Before implementing a function as a builtin, always check what module the test `use`s and where the function is actually defined.
- **Builtin functions must be listed in `raku-doc/doc/Language/perl-func.rakudoc`.** Only implement a function as a builtin if it appears in that file. If a function is not listed there, it is NOT a Raku builtin — it comes from a module (e.g. `Test`, `Test::Util`) and should be implemented in the appropriate module handler, not as a core builtin.
- Perl 6 (Raku) regex is not compatible with Perl 5 regex; never assume Perl 5 compatibility.
- Prefer ASCII in source files unless a specific Unicode feature is required.
- Do not rewrite or reformat unrelated code.
- Do not use stubs, hardcoded outputs, or early returns to make tests pass.
- Do NOT commit directly to the main branch. Always create a feature branch and open a pull request.
- Do not create PRs or Issues against Raku org repositories (including `roast` and `raku-doc`) from this workspace.
- PR workflow:
  1. Create a feature branch from main: `git checkout -b <branch-name>`
  2. Commit changes to the feature branch.
  3. Push and open a PR with `gh pr create`.
  4. Enable auto-merge: `gh pr merge --auto --squash <pr-number>`.
  5. CI (GitHub Actions) runs `make test` and `make roast`. The PR merges automatically when CI passes.
- If CI fails, fix the issue on the same branch, push again, and wait for CI to pass.
- Write all documents, code comments, and commit messages in English.
- Do not use `echo`, `cat`, `printf`, or heredoc via Bash to create files. Always use the Write tool.
- Temporary test scripts must be written to `./tmp/` (project-local, gitignored) using the Write tool. Never write to `/tmp/` or `/tmp/claude-1000/`.
- Do not use `cat`, `head`, `tail`, or `sed` via Bash to read files. Always use the Read tool.
- Do not use `grep`, `rg`, or `find` via Bash to search files. Always use the Grep and Glob tools.

## Reference implementation

- `raku` is available on this system. Use `raku -e '<code>'` to check expected behavior when the spec is unclear.
- When investigating a roast test, always run it with `raku` first to see the expected output before comparing with mutsu.
- Design docs: `./old-design-docs/`
- Raku language documentation: `./raku-doc/` — consult these docs when the language spec or behavior is unclear. See the section below for a detailed guide to the most useful files.

## Raku documentation guide (`raku-doc/`)

All documentation is under `raku-doc/doc/` in `.rakudoc` format (Pod6 markup, readable as plain text).

### Language reference (`raku-doc/doc/Language/`)

Core syntax and semantics — consult these when implementing or debugging language features:

- **`syntax.rakudoc`** — General syntax: literals, identifiers, statements, comments, special variables
- **`operators.rakudoc`** (135 KB) — **All operators with precedence levels**, associativity, and examples. Essential reference for parser precedence implementation.
- **`control.rakudoc`** — Control flow: if/unless/with/without, for, while, loop, given/when, repeat, gather/take, supply/react/whenever
- **`functions.rakudoc`** — Function definition, calling conventions, return handling
- **`signatures.rakudoc`** — Parameter syntax, types, constraints, slurpy params, destructuring
- **`variables.rakudoc`** — Sigils (`$`, `@`, `%`, `&`), twigils, special variables, dynamic scope
- **`regexes.rakudoc`** (120 KB) — **Complete Raku regex syntax**. Anchors, quantifiers, captures, lookahead/lookbehind, character classes, alternation, conjunction, etc.
- **`grammars.rakudoc`** — Grammar, token, rule, regex declarators; actions; `TOP` method; inheritance
- **`grammar_tutorial.rakudoc`** — Step-by-step grammar tutorial with practical examples
- **`quoting.rakudoc`** — Quoting constructs: `q//`, `qq//`, heredocs, word quoting (`<...>`, `qw`), interpolation rules
- **`objects.rakudoc`** — OOP: classes, roles, attributes, methods, inheritance, composition, MRO
- **`classtut.rakudoc`** — Class tutorial with practical examples
- **`typesystem.rakudoc`** — Type system: type objects, coercions, subsets, where clauses
- **`subscripts.rakudoc`** — Positional and associative subscript syntax (postcircumfix `[]`, `{}`, `<>`)
- **`list.rakudoc`** — Lists, arrays, sequences, flattening, itemization
- **`containers.rakudoc`** — Scalars, arrays, hashes as containers; binding vs assignment
- **`contexts.rakudoc`** — Sink, boolean, string, numeric, list context coercion
- **`terms.rakudoc`** — Term syntax: self, now, time, rand, empty, etc.
- **`brackets.rakudoc`** — Bracket pairs and nesting rules
- **`phasers.rakudoc`** — BEGIN, CHECK, INIT, END, ENTER, LEAVE, KEEP, UNDO, FIRST, NEXT, LAST, PRE, POST, QUIT, CLOSE, COMPOSE
- **`statement-prefixes.rakudoc`** — `do`, `try`, `quietly`, `gather`, `lazy`, `eager`, `hyper`, `race`, `sink`, `react`, `supply`
- **`traits.rakudoc`** — `is`, `does`, `handles`, `of`, `returns`, `will` trait modifiers
- **`slangs.rakudoc`** — Slang mechanism details (sub-language switching)
- **`setbagmix.rakudoc`** — Set, Bag, Mix types and operations
- **`numerics.rakudoc`** — Numeric types: Int, Num, Rat, FatRat, Complex; coercion rules
- **`exceptions.rakudoc`** — Exception handling: try, CATCH, die, fail, X:: types
- **`packages.rakudoc`** — Packages, modules, classes, roles as package types
- **`traps.rakudoc`** — Common pitfalls and gotchas (useful for understanding edge cases)

### Type reference (`raku-doc/doc/Type/`)

Per-type method documentation — consult when implementing methods on specific types:

- **`Test.rakudoc`** — **Test module specification**: `plan`, `ok`, `nok`, `is`, `isnt`, `is-deeply`, `is-approx`, `like`, `unlike`, `cmp-ok`, `isa-ok`, `does-ok`, `can-ok`, `dies-ok`, `lives-ok`, `eval-dies-ok`, `eval-lives-ok`, `throws-like`, `subtest`, `skip`, `todo`, `pass`, `flunk`, `bail-out`, `done-testing`, `diag`
- **`Grammar.rakudoc`** — Grammar type methods: `parse`, `parsefile`, `subparse`
- **`Match.rakudoc`** — Match object methods and structure
- **`Regex.rakudoc`** — Regex type documentation
- **`Str.rakudoc`** (57 KB) — String methods (comprehensive)
- **`List.rakudoc`** (54 KB) — List methods
- **`Any.rakudoc`** (57 KB) — Any type methods (inherited by most types)
- **`Cool.rakudoc`** (52 KB) — Cool type coercion methods
- **`Hash.rakudoc`** — Hash methods
- **`Array.rakudoc`** — Array methods
- **`Int.rakudoc`**, **`Num.rakudoc`**, **`Rat.rakudoc`**, **`Complex.rakudoc`** — Numeric type methods
- **`Range.rakudoc`** — Range type methods
- **`Junction.rakudoc`** — Junction type (any, all, one, none)
- **`IO/Path.rakudoc`** and related `IO/` types — File I/O methods
- **`independent-routines.rakudoc`** (57 KB) — Built-in functions not tied to a specific type (e.g., `say`, `print`, `put`, `note`, `dd`, `exit`, `sleep`, `elems`, `keys`, `values`, etc.)

### How to use

- When implementing a new operator or fixing precedence: read `Language/operators.rakudoc`
- When implementing regex features: read `Language/regexes.rakudoc`
- When implementing grammar support: read `Language/grammars.rakudoc` and `Type/Grammar.rakudoc`
- When implementing Test module functions: read `Type/Test.rakudoc`
- When implementing methods on a type: read the corresponding `Type/<TypeName>.rakudoc`
- When parser behavior is unclear: read `Language/syntax.rakudoc` and `Language/traps.rakudoc`

## Roast (official Raku test suite)

- The ultimate goal is to pass ALL roast tests. Use `pick-next-roast.sh` to select which test to work on (see "Roast test prioritization" below).
- `roast/` is read-only; never modify files under `roast/`.
- `TODO_roast/` tracks per-file pass/fail status (split by synopsis number). Mark a test `[x]` only when **all** subtests pass.
- When a test file has known partial failures, add indented notes under its entry describing the blockers.
- Do not add a roast test to the whitelist unless `prove -e 'target/debug/mutsu' <file>` exits cleanly.
- Keep `roast-whitelist.txt` sorted (`LC_ALL=C sort -c roast-whitelist.txt`); CI fails if it is not sorted.
- Never add special-case logic, hardcoded results, or test-specific hacks just to pass a roast test. Every fix must be a genuine, general-purpose improvement.
- When the expected behavior is unclear, consult `./old-design-docs/` for the original Raku language specification.
- When investigating a roast test and deciding to defer it, always record the reason for failure in the relevant file under `TODO_roast/`.
- Roast is the authoritative spec. If passing a roast test requires changing a local test under `t/`, update the local test.
- When `make roast` shows failures in whitelisted tests, investigate each failure — do NOT dismiss them as "pre-existing".
- Never remove a previously passing test from the whitelist due to a regression. If a change causes a whitelisted test to fail, fix the regression so the test passes again.
- When a roast test requires solving multiple unrelated prerequisites: fix what you can, update the relevant file under `TODO_roast/`, and move on.

## Working on complex features

- Do NOT fear complex cases. Every feature must eventually be implemented fully.
- Deferring hard work does not make it go away — tackle difficult features head-on.
- When a roast test requires a complex feature (e.g., attribute traits, arbitrary regex delimiters, module precompilation), implement it rather than skipping to easier tests.
- When a single test requires implementing multiple unrelated features, implement them all in the same PR. Large PRs are acceptable when the test demands it.

## Roast test prioritization

- Run `./scripts/roast-history.sh` to generate per-file category lists under `tmp/`:
  - `tmp/roast-panic.txt` — Rust panics (highest priority)
  - `tmp/roast-timeout.txt` — timeouts
  - `tmp/roast-error.txt` — no valid TAP plan
  - `tmp/roast-fail.txt` — some subtests failing
  - `tmp/roast-pass.txt` — fully passing
- Priority order: panic → timeout → error/fail.
- **Raku filter**: Run `./scripts/roast-raku-check.sh` to generate `tmp/roast-raku-pass.txt` (tests listed in `roast/spectest.data`). When this file exists, `pick-next-roast.sh` only shows tests that are expected to pass on Rakudo, skipping tests that raku itself cannot pass. The file is cached — re-run the script to refresh.
- **ALWAYS use `./scripts/pick-next-roast.sh -n N` to select the next test(s).** Do NOT manually browse, scan, or read roast test files to choose which test to work on. The script's selection is final — work on whatever it returns, regardless of perceived difficulty.
- Do NOT skip a test because it looks hard or requires implementing a complex feature. If `pick-next-roast.sh` returns it, work on it.
- Do NOT run multiple roast tests to "assess" which one is closest to passing. This is cherry-picking and is forbidden.
- The goal is broad language coverage, not gaming the pass count.
- After making changes, always run `./scripts/roast-history.sh` to regenerate category lists and check for newly passing tests. Compare the new `tmp/roast-pass.txt` against the whitelist to find tests to add.

## Checking `make roast` results

```
make roast 2>&1 | grep -E "(FAILED|Failed)" | head -20
make roast 2>&1 | grep -E "(not ok|FAILED|Failed|Wstat)" | head -20
```

## Running mutsu safely

mutsu is under active development — parsing or execution can hang. **Always use `timeout`** when running mutsu:

```
timeout 30 target/debug/mutsu <file>
timeout 30 target/debug/mutsu -e '<code>'
timeout 30 target/debug/mutsu --dump-ast <file>
```

## Investigating a failing roast test

Before writing any code, always investigate the test in this order:

1. **Run with `raku`** to see the expected output: `raku <roast-test-path>`
2. **Dump AST with `raku`** (if needed): `raku --target=ast -e '<relevant code>'`
3. **Dump AST with `mutsu`**: `timeout 30 target/debug/mutsu --dump-ast <roast-test-path>`
4. **Run with `mutsu`**: `timeout 30 target/debug/mutsu <roast-test-path>`
5. Compare outputs to identify what mutsu is doing wrong, then fix the interpreter.

## AI fleet operations

Roast test fixing is automated via a fleet of sandboxed AI agents. The parent AI (or human) launches workers and a supervisor, then monitors their progress.

### Scripts

- **`ai-sandbox.sh`** — Creates an isolated sandbox (via `bwrap`) with a fresh git clone of the repo per branch. Runs `claude`, `codex`, or `bash` inside the sandbox. Sandboxes share a cargo build cache under `.git/sandbox/.shared-target/`.
- **`ai-run-roast.sh`** — Takes a single roast test file path, builds a prompt with investigation steps, and runs an agent in a sandbox to fix it. Retries up to 3 times on failure.
- **`ai-next-roast.sh`** — Continuously picks random failing roast tests (via `pick-next-roast.sh`), coordinates with other instances via `wip.txt` to avoid duplicate work, and delegates each test to `ai-run-roast.sh`.
- **`ai-supervisor.sh`** — Monitors open PRs for merge conflicts and CI failures. Dispatches agents to rebase/fix them. When idle, triggers roast history updates. Polls every 10 minutes.

### Starting the fleet (tmux)

Each worker and the supervisor should run in its own tmux window so that logs are visible and manageable.

```bash
# Launch N workers in separate tmux windows
tmux new-window -n 'codex-1' -d 'cd /path/to/mutsu && dotenvx run -- ./scripts/ai-next-roast.sh --agent codex'
tmux new-window -n 'codex-2' -d 'cd /path/to/mutsu && dotenvx run -- ./scripts/ai-next-roast.sh --agent codex'
tmux new-window -n 'codex-3' -d 'cd /path/to/mutsu && dotenvx run -- ./scripts/ai-next-roast.sh --agent codex'
tmux new-window -n 'claude'  -d 'cd /path/to/mutsu && dotenvx run -- ./scripts/ai-next-roast.sh --agent claude'

# Launch 1 supervisor
tmux new-window -n 'supervisor' -d 'cd /path/to/mutsu && dotenvx run -- ./scripts/ai-supervisor.sh --agent codex'
```

### Monitoring

- **List windows**: `tmux list-windows -F '#{window_index} #{window_name}'`
- **Read worker output**: `tmux capture-pane -t ":N" -p -S -30` (N = window index)
- **Worker processes**: `ps aux | grep ai-next-roast`
- **PR progress**: `gh pr list --state open`
- **Supervisor status**: `dotenvx run -- ./scripts/ai-supervisor.sh --list`
- **Agent logs** (claude): pipe through `python3 scripts/stream-json-pretty.py`
- **Restart dead workers**: `tmux send-keys -t ":N" 'dotenvx run -- ./scripts/ai-next-roast.sh --agent codex' Enter`

### Parent AI responsibilities

- Periodically run `tmux capture-pane -t ":N" -p -S -30` for each worker window to check progress
- Check that child processes are alive (`tmux list-panes -t ":N" -F '#{pane_pid} #{pane_dead}'`) and restart any that died
- Review captured logs and identify patterns in failures
- Improve prompts and scripts based on observed failure modes
- Record failure reasons in `TODO_roast/` to accumulate knowledge
- Monitor `wip.txt` for stale entries (agents that died mid-task)

### Stopping

```bash
touch tmp/.stop
```

All scripts check for `tmp/.stop` and exit gracefully when it appears.

## Test::Util function workout

When the user says **"Test::Util workout"** (or similar), execute this workflow:

1. Check `roast/packages/Test-Helpers/lib/Test/Util.rakumod` for the list of exported functions.
2. Check `t/` for existing `test-util-*.t` and `is-run.t` etc. to see which functions already have tests.
3. Pick **one** unimplemented or undertested function. Once chosen, do NOT switch to a different one.
4. Write a test file `t/<function-name>.t` exercising the function with multiple cases (basic usage, edge cases, combined checks).
5. Run the test with `timeout 30 target/debug/mutsu t/<function-name>.t` to see what fails.
6. Fix the interpreter to make the test pass. When the spec is unclear, check behavior with `raku -e '<code>'` and consult `raku-doc/`.
7. Run `make test` and `make roast` to ensure no regressions.
8. Create a feature branch, commit, push, and open a PR (follow PR workflow above).
9. Enable auto-merge: `gh pr merge --auto --squash <pr-number>`.

Key rules:
- The function implementation lives in `src/runtime/test_functions.rs` (not as a builtin).
- If fixing the function requires adding a new language feature (e.g., `exit_code` support), implement it properly — no stubs or hacks.
- Test::Util functions are defined in `roast/packages/Test-Helpers/lib/Test/Util.rakumod`. Always read the source to understand the expected behavior before implementing.

## Debugging guidelines

- Do NOT use printf debugging (eprintln! → build → check → repeat). Rust builds are slow.
- Preferred approaches in order:
  1. **AST dump**: `timeout 30 ./target/debug/mutsu --dump-ast <file>` or `--dump-ast -e '<code>'`
  2. **Trace logs**: `timeout 30 env MUTSU_TRACE=1 ./target/debug/mutsu <file>` (filter: `MUTSU_TRACE=eval` or `MUTSU_TRACE=parse,vm`)
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

**Implication for mutsu**: The parser does not natively support slang switching. As we add `grammar`/`token`/`rule` support (which let users define custom grammars), we may need an architecture that can switch parsing modes contextually. Keep the parser modular so that individual sub-parsers (regex, quote, etc.) can be extracted and reused in a future architecture.

## Conventions

- Keep each Rust source file under 500 lines. When a file exceeds 500 lines, split it into smaller modules immediately — do not defer.
- Write feature tests using prove (`t/*.t`).
- Use Rust unit tests (`#[test]`) for internal components like parser and runtime helpers.
- Every feature addition must include tests.
- When implementing a temporary workaround or shortcut instead of the correct solution, always leave a `// TODO:` comment explaining what the correct approach would be and why the current implementation is insufficient. This ensures technical debt is visible and trackable.
