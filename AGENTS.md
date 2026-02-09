# AGENTS

This repo is a Rust implementation of a minimal Raku (Perl 6) compatible interpreter.

## Working agreements
- Keep changes small and well-documented; prefer incremental features aligned with roast tests.
- Prefer ASCII in source files unless a specific Unicode feature is required.
- Do not rewrite or reformat unrelated code.
- Do not use stubs, hardcoded outputs, or early returns to make tests pass.
- Commit directly to the main branch. Do not use feature branches.
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
- Full roast run: `tools/run_all_roast.sh` (KPI: pass count)
  - Pass `--save` to append results to `tools/roast_results.log`
  - After adding features, run `tools/run_all_roast.sh --save` to record the pass count trend

## Spec sources
- Roast tests live at `../roast/`.
- Design docs live at `../old-design-docs/`.

## Development workflow
1. Implement Raku language features straightforwardly. Do not optimize specifically to increase the roast pass count.
2. Write tests (prove-based) for every feature implemented.
3. Focus on rapid feature development in early phases. Do not run roast after every change.
4. Roast runs (`tools/run_all_roast.sh --save`) are for milestone checks only (e.g., end of a phase or major batch of features).

## Conventions
- Add small, focused tests for each new syntax feature.
- Keep the parser and evaluator readable; comment only non-obvious logic.
- Write tests using prove. Do not use Rust integration tests in `tests/*.rs` for new coverage.
- Every feature addition must include tests. A feature without tests is considered incomplete.
