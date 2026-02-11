# AGENTS

This repo is a Rust implementation of a minimal Raku (Perl 6) compatible interpreter.

## Working agreements
- Keep changes small and well-documented
- Perl 6 (Raku) regex is not compatible with Perl 5 regex; never assume Perl 5 compatibility.
- Prefer ASCII in source files unless a specific Unicode feature is required.
- Do not rewrite or reformat unrelated code.
- Do not use stubs, hardcoded outputs, or early returns to make tests pass.
- Commit directly to the main branch. Do not use feature branches.
- Push to remote after completing work.
- Write all documents (CLAUDE.md, TODO.md, etc.), code comments, and commit messages in English.

## Layout
- `src/` holds the interpreter implementation.
- `bin/` holds the CLI entrypoint (if needed).
- `tests/` holds local Rust tests (unit/integration).

## Build & run
- Build: `cargo build`
- Run: `cargo run -- <file.p6>`
- Test: `cargo test`
- Full test (cargo + prove): `make test`

## Spec sources
- Design docs live at `./old-design-docs/`.
- The official Raku test suite (roast) is available at `./roast/` as a git submodule.

## Roast (official Raku test suite)
- `roast/` contains the upstream Raku spec tests. It is read-only; never modify files under `roast/`.
- `TODO_roast.md` tracks per-file pass/fail status. Mark a test `[x]` only when **all** of its subtests pass.
- When a test file has known partial failures, add indented notes under its entry describing the blockers.
- `roast-whitelist.txt` lists tests that pass completely. `make roast` runs only these via prove.
- After fixing a bug or adding a feature, check whether any roast tests now pass and update the whitelist and TODO accordingly.
- Do not add a roast test to the whitelist unless `prove -e 'cargo run --' <file>` exits cleanly (all subtests pass).
- Roast tests may use constructs the interpreter does not yet support. Prefer fixing the interpreter over skipping tests.
- Never add special-case logic, hardcoded results, or test-specific hacks just to pass a roast test. Every fix must be a genuine, general-purpose improvement to the interpreter.
- When the expected behavior of a roast test is unclear, consult `./old-design-docs/` for the original Raku language specification before implementing.
- When investigating a roast test and deciding to defer it, always record the reason for failure in `TODO_roast.md` under the corresponding entry. Even if a test is not fixed now, the diagnosis is valuable for future work.

## Conventions
- Add small, focused tests for each new syntax feature.
- Keep the parser and evaluator readable; comment only non-obvious logic.
- Write tests using prove. Do not use Rust integration tests in `tests/*.rs` for new coverage.
- Every feature addition must include tests. A feature without tests is considered incomplete.
