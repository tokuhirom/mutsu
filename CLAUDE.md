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

This is a **bytecode VM** architecture. The VM should handle ALL operations natively via compiled bytecode. The tree-walking `Interpreter` (`runtime/`) exists as legacy code that is being eliminated.

**IMPORTANT: Do NOT add new interpreter fallbacks from the VM.** When implementing a new feature:
- Implement it in the compiler (`compiler/`) to emit bytecode, and in the VM (`vm/`) to execute it.
- Do NOT call `interpreter.call_method_with_values()`, `interpreter.run_instance_method()`, or similar interpreter methods from VM code for new features.
- Existing interpreter fallbacks are technical debt to be eliminated, not a pattern to follow.
- If you must use the interpreter as a temporary measure, leave a `// TODO: compile to bytecode` comment.

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
- `TODO_roast/BLOCKERS.md`: Feature-level blocker analysis — groups all failing tests by the missing feature that blocks them, ordered by impact. Use this to decide which feature to implement next for maximum roast progress.
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
  6. After creating a PR, enable auto-merge — then **watch its CI in the background** (a `run_in_background` bash poll loop on `gh pr checks <pr-number>` that exits when no check is `pending`, so the harness notifies you on completion). Do NOT use a *foreground* `gh pr checks --watch` (it blocks ~13 min and wastes the session). If there is genuinely-independent, non-conflicting work to do (a different module/area), do it while the background watch runs; but when there isn't, the background watch is the productive thing — it surfaces a CI failure immediately so you can fix-forward, instead of leaving a red PR sitting unnoticed.
  7. **Once the PR is out and the background CI watch is running, decide the next slice before going idle.** Do not stop and wait for the merge with nothing queued. Re-read the relevant ledger/PLAN (e.g. `docs/vm-interpreter-fallback-ledger.md`, `PLAN.md`) and pick the next concrete unit of work — either start it on a fresh branch off `main` (if it is independent of the open PR), or, when the next step is genuinely a strategic fork, lay out the options and confirm direction with the user. The merge of the current PR should never be the thing that "unblocks thinking about what's next" — that thinking happens now, in parallel with the watch.
- **Watch the PR's CI in the background; don't *foreground-block* on it.** The old "fire-and-forget, just keep working" advice assumed there was always parallel non-conflicting work queued up. That era is over — in the final stretch there is usually little independent work left, so the most efficient thing after pushing + enabling auto-merge is to **background-watch the PR you just opened** (poll loop as above) rather than walking away from it. A red CI caught and fixed within minutes beats one discovered an hour later. Only skip the watch when you have a concrete, independent next task; even then, a background watch costs nothing and still notifies you. Auto-merge still lands the PR on its own when CI goes green.
- If CI fails, fix the issue on the same branch and push again (the background watch will notify you; re-watch after pushing).
  - **Known flaky:** `roast/S02-names-vars/perl.t` intermittently exits 255 with `Failed: 0` (plan incomplete) — the typed-container alloc/hash-order flaky documented in PLAN.md. Passes ~80% of runs. If CI fails *only* on this with `Failed: 0`, re-trigger CI (push an empty commit) rather than treating it as a regression; confirm locally with a release build a few times first.
- **Never close a PR without preserving its knowledge.** If a PR has rebase conflicts, rebase it (manually or with an agent that reads the PR diff via `gh pr diff <number>`). The PR diff itself is the best documentation of the change — do not just close it and write a summary. Reopen and fix it, or have a new agent read the diff and re-implement on a fresh branch.
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

- The ultimate goal is to pass ALL roast tests.
- **Task selection: PLAN.md and BLOCKERS.md driven.** Do NOT randomly pick roast tests. Instead:
  1. Check **PLAN.md** for the current quarter's priorities and work on those first.
  2. Check **TODO_roast/BLOCKERS.md** for feature-level blocker analysis — implement the highest-impact missing feature to unblock the most tests at once.
  3. `pick-next-roast.sh` is a fallback for AI fleet workers, not the primary way to choose work.
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

**Primary: PLAN.md → BLOCKERS.md → then individual tests.**

The project is in its final stretch. Work should be driven by strategic priorities, not random test selection:

1. **PLAN.md priorities first.** Check the current quarter's section for high-impact tasks (e.g., exception types, performance, module compatibility). These are chosen because they unblock many tests or advance project goals.
2. **BLOCKERS.md for roast work.** When working on roast, consult `TODO_roast/BLOCKERS.md` which groups failing tests by the missing feature. Implement features that unblock the most tests (e.g., "Exception Types" blocks 22 tests, "Threading" blocks 31).
3. **Roast diagnostic tools** (for status tracking, not task selection):
   - Run `./scripts/roast-history.sh` to generate per-file category lists under `tmp/`:
     - `tmp/roast-panic.txt` — Rust panics (highest priority to fix)
     - `tmp/roast-timeout.txt` — timeouts
     - `tmp/roast-error.txt` — no valid TAP plan
     - `tmp/roast-fail.txt` — some subtests failing
     - `tmp/roast-pass.txt` — fully passing
   - After making changes, run `roast-history.sh` to check for newly passing tests.
4. **`pick-next-roast.sh`** is available as a fallback for AI fleet workers or when all PLAN.md/BLOCKERS.md tasks are in progress. It is NOT the primary way to choose work.

- Do NOT skip a task because it looks hard. Tackle difficult features head-on.
- Do NOT cherry-pick easy tests to game the pass count. The goal is implementing missing features that have broad impact.

## Trust the main branch

The `main` branch is protected by GitHub branch protection rules — only PRs that pass CI (`make test` + `make roast`) can be merged. **Do NOT waste time checking whether a failing test also fails on `main`.** If a test fails on your feature branch, the problem is in your changes, not in `main`. Checking out `main` or running tests against it to "verify" is pointless and wastes AI resources.

## Refactor boldly — CI + roast are the safety net

CI (`make test` + the extensive `make roast` spec suite) gates every merge, and the roast suite is comprehensive. **Do NOT hide behind "this is too risky, I'll ship a tiny slice / just document the design."** Over-caution that avoids the actual architectural change is a worse failure than a bold change that CI catches. When the right fix is a substantial, high-blast-radius refactor (e.g. collapsing the `locals`↔`env` dual store, reworking dispatch, changing core data structures), **do the real change**:

- Make the architectural change in full, not as a string of timid micro-gates that never reach the goal.
- Validate locally with `make test` + a few targeted tests, fix what those catch, then push and let CI's full roast run be the comprehensive safety net.
- If CI fails, fix forward on the same branch and re-run. A red CI on a feature branch is cheap and expected during a big refactor — it cannot reach `main`.
- Prefer one coherent architectural PR over ten micro-PRs that collectively dance around the real problem.

The goal is essential architectural improvement, not the appearance of progress through small safe diffs.

## Known flaky tests

Some tests are genuinely non-deterministic (concurrency/timing/CI-load sensitive) and fail intermittently. When a `make roast` / `make test` failure is **only** in the list below and your change is unrelated (e.g. an operator/parser fix), treat it as flaky: re-run the single file a few times before assuming a regression. Do **not** remove it from the whitelist.

- `roast/S17-supply/batch.t` and other `S17-supply/*` / `S17-*` concurrency tests — fail occasionally, pass on retry.
- `t/lock.t` — lock contention / occasional `exit 255` timeout.
- `roast/S02-types/hash.t`, `roast/S09-typed-arrays/hashes.t` — CI-load-sensitive **timeouts** (bad plan / `exit 255` with `Failed: 0`); pass reliably locally. A *concrete subtest* failure here is NOT load-related — see triage below.

Also: before a local `make roast`, `rm -f temp-file-RT-126006-test` — a stale temp file left by an interrupted `roast/S32-io/spurt.t` makes that test abort with "cannot run test while file ... exists".

### Triaging a suspected-flaky failure — don't mislabel a real bug

"Flaky" is a claim about *non-determinism*; verify it before trusting it. A failure that reproduces every run is a real bug to fix, not noise to skip. `t/wrap.t`, `t/placeholder.t`, and `t/tail-function.t` sat here for months labeled "flaky / pre-existing" when all three were **deterministic correctness bugs** (closure-capture env writeback, scope-lost Seq iterator, missing `%_` placeholder capture — fixed in #2629 / #2630 / #2632). Before adding or trusting a flaky label:

1. **Re-run the single file ~5× in a release build** (`cargo build --release && prove -e target/release/mutsu <file>`). Fails every time → deterministic → fix it, don't skip it.
2. **Read the failure shape.** A *timeout* / `exit 255` with `Failed: 0` (bad plan, ran fewer than planned) is plausibly load/timing. A *concrete subtest* failure (`Failed: N`, a real `not ok` assertion) is almost always a logic bug — even on a "known flaky" file. Investigate the subtest.
   - **BUT `exit 255` + `Failed: 0` is NOT automatically flaky.** "Ran N of M, Failed: 0" also happens when your *own* change throws an unexpected exception **mid-file** (e.g. a false-positive `X::Redeclaration` / `X::Assignment::RO`), which aborts the rest of the file with `Runtime error: Test failures` — looking exactly like a timeout. **The tell:** it reproduces *deterministically* on your branch but NOT on `main`, and the `(N+1)`th test is precisely the construct your change touches. Before declaring flaky, run the exact file on your branch vs `main` (`prove -e target/debug/mutsu <file>`); if your branch aborts and `main` completes, it is a real regression you introduced — fix it, do not re-trigger CI. (Seen in Tier-2: a sigil-blind `constant` redeclaration check aborted `S06-operator-overloading/sub.t`; a literal-LHS `s///` RO check aborted `S05-metasyntax/regex.t` on `TR///`.)
3. **debug vs release.** A debug-only timeout on a heavy test can be load; a failure that reproduces in *release* is real (CI uses release).
4. Only label flaky if it actually **passes on retry**; note the pass/fail ratio when you do.

Caveat: the `t/` TAP suite is **non-fatal** in CI (`prove ... t/ || echo "non-fatal; roast is authoritative"`), so a deterministic `t/` failure stays green in CI and can hide indefinitely. Don't rely on CI to surface `t/` regressions — check `tmp/make-test.log` locally.

## Delegate the full roast run to CI

Running the entire roast suite locally is wasteful and slow — **let CI run the full `make roast`.** Locally, run only the specific tests relevant to your change:

- Run individual roast tests with `prove -e 'target/debug/mutsu' roast/<path>.t`, or the exact files you touched / suspect regressed.
- CI runs `make test` and `make roast` on a clean machine with the **release build** (`cargo build --release`, `MUTSU_BIN=target/release/mutsu`). Local debug builds are much slower, so a local timeout on a heavy test does not necessarily indicate a real failure — confirm with a release build (`target/release/mutsu`) before assuming a regression, and otherwise trust CI's verdict.
- Push the branch and rely on CI for the comprehensive roast result rather than running the whole suite locally.

### Use the DEBUG build to iterate on `MUTSU_VM_STATS` counters — release is for wall-clock only

The `MUTSU_VM_STATS=1` dual-store counters (`locals_pulls`, `env_flushes`, `env_deep_copies`, `clone_env`, ...) are **deterministic and independent of the optimization level** — they count VM events, not time. So when tuning a perf/decoupling change against those counters, **iterate with the debug build** (`cargo build`, ~30-70s) and read the counters off `target/debug/mutsu`; the numbers are identical to release. Reserve the slow `cargo build --release` (which here also emits debuginfo and can take 10-15 min) for the **final wall-clock measurement** only. Do NOT default to release just because the task is perf-related — that wastes ~10× the build time per iteration for byte-identical counter output.

## Checking `make test` / `make roast` results

`make test` and `make roast` automatically save their full output to log files via `tee`:
- `make test` → `tmp/make-test.log`
- `make roast` → `tmp/make-roast.log`

**After running `make test` or `make roast`, always use the Grep tool on the log file instead of re-running the command.** Do NOT re-run `make test` or `make roast` just to grep the output — use the saved log file.

```
# Use the Grep tool on these files:
# tmp/make-test.log
# tmp/make-roast.log
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

## Worktree cleanup

When using `isolation: "worktree"` agents, stale worktrees accumulate under `.claude/worktrees/` and can consume hundreds of GB. **Clean up worktrees at least once per hour** during long sessions:

```bash
# Remove all agent worktrees except currently active ones
cd .claude/worktrees/
for d in agent-*; do
  git -C <repo-root> worktree remove --force ".claude/worktrees/$d" 2>/dev/null
done
git worktree prune
```

Before cleanup, check which agents are still running and exclude their worktrees. After cleanup, verify disk usage with `du -sh .claude/worktrees/`.

## LXC container environment

This development environment runs inside a dedicated mutsu LXC container. The container may be destroyed at any time — always commit important changes and push PRs promptly.

- **codex can run in YOLO mode** (`codex exec --dangerously-bypass-approvals-and-sandbox`) since the container itself provides isolation. No bwrap sandbox is needed.
- **`pick-next-roast-easy.sh`** — Picks easy (fail-only, short) roast tests suitable for codex. Used by codex workers to avoid timeout/parse-error tests that are too hard.
- **codex worker**: Run via `tmp/codex-loop.sh` which uses `pick-next-roast-easy.sh` + codex YOLO mode.

## AI fleet operations

Roast test fixing is automated via a fleet of sandboxed AI agents. The parent AI (or human) launches workers and a supervisor, then monitors their progress.

### Scripts

- **`ai-sandbox.sh`** — Creates an isolated sandbox (via `bwrap`) with a fresh git clone of the repo per branch. Runs `claude`, `codex`, or `bash` inside the sandbox. Copies parent repo's `target/` as build cache on first clone. Sandboxes persist across runs (no `--recreate` by default) so interrupted agents can resume.
- **`ai-run-roast.sh`** — Takes a single roast test file path, builds a prompt with investigation steps, and runs an agent in a sandbox to fix it. If the sandbox has uncommitted changes from a previous interrupted session, the prompt instructs the agent to continue from where it left off. Retries up to 3 times on failure.
- **`ai-next-roast.sh`** — Continuously picks random failing roast tests (via `pick-next-roast.sh`), coordinates with other instances via `wip.txt` to avoid duplicate work, and delegates each test to `ai-run-roast.sh`.
- **`ai-supervisor.sh`** — Monitors open PRs for merge conflicts and CI failures. Dispatches agents to rebase/fix them. When idle, triggers roast history updates. Polls every 10 minutes.
- **`ai-fleet.sh`** — Fleet manager. Monitors tmux windows with the `fleet:` prefix and automatically restarts dead workers. Fleet composition is configured via CLI options.
- **`tmux-status.sh`** — Quick status display of all tmux windows (pid, alive/dead, last N lines of output). Usage: `./scripts/tmux-status.sh [lines]`

### Fleet manager

`ai-fleet.sh` manages the fleet automatically. It monitors tmux windows with the `fleet:` prefix and restarts dead workers.

```bash
# Start with defaults (codex x2, claude x1, supervisor using codex)
./scripts/ai-fleet.sh

# Custom fleet composition
./scripts/ai-fleet.sh --codex 3 --claude 2 --supervisor claude

# No supervisor
./scripts/ai-fleet.sh --codex 2 --claude 1 --no-supervisor

# Check status once without looping
./scripts/ai-fleet.sh --once

# Dry-run: show what would be launched
./scripts/ai-fleet.sh --once --dry-run

# Gracefully stop specific workers (fleet manager will restart them)
./scripts/ai-fleet.sh --stop supervisor
./scripts/ai-fleet.sh --stop codex        # stops all codex workers
./scripts/ai-fleet.sh --stop all
```

Default fleet composition (override with CLI options):
- `--codex 2` — 2 codex workers
- `--claude 1` — 1 claude worker
- `--supervisor codex` — supervisor using codex agent

### Monitoring

- **Fleet status**: `./scripts/ai-fleet.sh --once`
- **Read worker output**: `tmux capture-pane -t "fleet:codex:1" -p -S -30`
- **PR progress**: `gh pr list --state open`
- **Supervisor status**: `dotenvx run -- ./scripts/ai-supervisor.sh --list`

### Stopping

```bash
# Stop all agents (fleet manager also exits)
touch tmp/.stop

# Stop a specific agent by PID
touch tmp/.stop.<PID>
```

All scripts check for `tmp/.stop` (all agents) and `tmp/.stop.<PID>` (specific agent) and exit gracefully. PID-specific stop files are automatically cleaned up on exit.

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

- **Always run `cargo fmt` and `cargo clippy -- -D warnings` before committing.** These checks run as pre-commit hooks, but you must ensure your code passes them before creating a commit. Never commit unformatted code.
- Keep each Rust source file under 500 lines. When a file exceeds 500 lines, split it into smaller modules immediately — do not defer.
- Write feature tests using prove (`t/*.t`).
- Use Rust unit tests (`#[test]`) for internal components like parser and runtime helpers.
- Every feature addition must include tests.
- When implementing a temporary workaround or shortcut instead of the correct solution, always leave a `// TODO:` comment explaining what the correct approach would be and why the current implementation is insufficient. This ensures technical debt is visible and trackable.

## Project planning and news

- **PLAN.md** contains only **future tasks**. Keep it slim — no completed items. When a task is done, remove it from PLAN.md.
- **news/YYYY-MM.md** (e.g. `news/2026-05.md`) records what was accomplished each month. When completing a PLAN.md task, move its description and results to the current month's news file.
- PLAN.md links to `news/` for historical context. Do not duplicate completed work in both files.
- When starting a new month's work, create `news/YYYY-MM.md` if it doesn't exist.

## Agent workflow

- **Sub-agents are allowed (policy updated 2026-06-15).** Use them where they help — read-only fan-out searches (Explore), independent non-conflicting implementation slices, or sweeping across many files where you only need the conclusion. Be deliberate, not reflexive: Rust builds are expensive and each parallel worktree agent multiplies `cargo build`/`clippy`/`make test` cost and accumulates large `target/` worktrees, so reach for a sub-agent when the task genuinely fans out, not for trivial single-file work you can do inline. Read-only Explore/research agents are cheap; worktree-isolated build agents are not.
- Keep at most 4 concurrent agents (not more, to avoid quota exhaustion). Clean up worktrees per the "Worktree cleanup" section.
- **Task selection order:**
  1. PLAN.md current quarter priorities
  2. BLOCKERS.md highest-impact missing features
  3. Roast tests related to in-progress features
  4. `pick-next-roast.sh` only as a last resort
