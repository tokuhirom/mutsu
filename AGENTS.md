# AGENTS

This repo is a Rust implementation of a minimal Raku (Perl 6) compatible interpreter.

## Working agreements
- Keep changes small and well-documented; prefer incremental features aligned with roast tests.
- Prefer ASCII in source files unless a specific Unicode feature is required.
- Do not rewrite or reformat unrelated code.
- Do not use stubs, hardcoded outputs, or early returns to make tests pass.

## Layout
- `src/` holds the interpreter implementation.
- `bin/` holds the CLI entrypoint (if needed).
- `tests/` holds local Rust tests (unit/integration).

## Build & run
- Build: `cargo build`
- Run: `cargo run -- <file.p6>`
- Test: `cargo test`
- Full test (cargo + prove): `make test`
- Roast 全件実行: `tools/run_all_roast.sh` (KPI: pass数)
  - `--save` を付けると `tools/roast_results.log` に結果を追記
  - 機能追加後は `tools/run_all_roast.sh --save` を実行し、pass数の推移を記録すること

## Spec sources
- Roast tests live at `../roast/`.
- Design docs live at `../old-design-docs/`.

## Conventions
- Add small, focused tests for each new syntax feature.
- Keep the parser and evaluator readable; comment only non-obvious logic.
- テストは原則 prove (`tools/prove_existing_roast.sh`) で実装すること。`tests/*.rs` の Rust integration test は使わない。
- 機能を追加したら必ずテストも実装すること。テストなしの機能追加は不完全とみなす。
