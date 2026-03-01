# Repository Guidelines

## Project Structure & Module Organization
`mutsu` is a Rust-based Raku compatibility interpreter.

- `src/`: core implementation.
- `src/parser/`, `src/compiler/`, `src/vm/`, `src/runtime/`: main execution pipeline.
- `src/builtins/` and `src/value/`: built-in behavior and value/type handling.
- `t/`: primary TAP-style integration tests (`*.t`) run with `prove`.
- `tests/`: additional TAP test files used by Rust test flows.
- `docs/`: internal design and parser/GC notes.
- `roast/`, `raku-doc/`, `old-design-docs/`: git submodules used as spec/reference material.

## Build, Test, and Development Commands
- `cargo build`: compile the interpreter (`target/debug/mutsu`).
- `cargo test`: run Rust unit/integration tests.
- `make test`: run `cargo test`, build, then execute local TAP tests in `t/`.
- `make roast`: run whitelisted upstream roast tests from `roast-whitelist.txt`.
- `cargo fmt --all`: format Rust code.
- `cargo clippy -- -D warnings`: lint with warnings treated as errors.

If submodules are missing, run:
`git submodule update --init --recursive`

## Coding Style & Naming Conventions
- Follow `rustfmt` defaults (4-space indentation, standard Rust formatting).
- Keep clippy clean; CI enforces `-D warnings`.
- Use `snake_case` for Rust modules/functions/files, `CamelCase` for types/traits, `SCREAMING_SNAKE_CASE` for constants.
- Name TAP tests in `t/` as descriptive `kebab-case.t` (for example, `statement-modifiers.t`).

## Trust the Main Branch
- The `main` branch is protected by GitHub branch protection — only code that passes CI is merged.
- **Never** check whether a failing test also fails on `main`. If `make test` or `make roast` fails on your feature branch, the problem is in your changes. Do not waste time or resources switching to `main` to "verify".

## Testing Guidelines
- Add or update tests for every behavior change, especially parser/runtime fixes.
- Prefer targeted TAP regression tests in `t/` for language behavior changes.
- Run `make test` before opening a PR; run `make roast` when touching spec-facing behavior.
- Keep `roast-whitelist.txt` sorted (`LC_ALL=C sort -c roast-whitelist.txt`); CI fails if it is not sorted.
- No fixed coverage threshold is configured; rely on regression-focused test additions.

## Commit & Pull Request Guidelines
- Use short, imperative commit subjects; optional scope prefixes are common (for example, `parser: accept ...`, `compiler: add ...`, `Fix ...`).
- Keep commits focused on one logical change.
- PRs should include: concise problem statement, approach, and test evidence (`make test` / `make roast` results).
- Link related issues/PRs when applicable (for example, `(#150)`).
- Ensure CI passes format, clippy, unit tests, TAP tests, and roast checks before merge.

## External Repository Policy
- Do not create PRs or Issues against Raku org repositories (including `roast` and `raku-doc`) from this workspace.
