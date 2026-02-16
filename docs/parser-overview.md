# Parser Overview (`parser_nom`)

This document summarizes the current hand-written parser so contributors (and AI agents) can make focused changes without re-reading the entire implementation.

For ongoing refactoring policy and roadmap, see `docs/parser-improvement-plan.md`.

## Scope and entry points

- Runtime entry: `src/parse_dispatch.rs` -> `parse_source()`
- Parser entry: `src/parser_nom/mod.rs` -> `parse_program(input)`
- Statement loop: `src/parser_nom/stmt.rs` -> `program()` -> `stmt_list()` -> `statement()`
- Expression entry: `src/parser_nom/expr.rs` -> `expression()`

`parse_program()` also handles `=finish` splitting and returns `(Vec<Stmt>, Option<String>)`.

## File responsibilities

- `src/parser_nom/mod.rs`: top-level parse orchestration and parse error wrapping
- `src/parser_nom/parse_result.rs`: `PResult` / `PError` and tiny parser primitives
- `src/parser_nom/helpers.rs`: whitespace/comment/Pod skipping (`ws`, `ws1`)
- `src/parser_nom/stmt.rs`: statement grammar and statement-level dispatch order
- `src/parser_nom/expr.rs`: operator precedence climbing and expression combinators
- `src/parser_nom/primary.rs`: literals, identifiers/calls, vars, blocks, hashes, regex literals

## Statement dispatch order (important)

`statement()` in `src/parser_nom/stmt.rs` tries many forms in fixed order and returns the first match.
Order changes can alter parse behavior.

High-impact groups in current order:

1. Declarations/modules: `use`, `unit module`, `my`, `constant`, `class/role/grammar/subset/enum/has/does/proto/sub/method/token`
2. Output/conditionals/loops: `say/print/note`, `if/unless/with`, labeled loops, `for/while/until/loop/repeat`, `given/when/default`
3. Control and phasers: `return/last/next/redo/die/take/catch/control/phaser`
4. Reactive/test/package: `subtest/react/whenever/package`
5. Fallbacks: known-call stmt, assignment stmt, block stmt, expression stmt

## Expression precedence (low -> high)

In `src/parser_nom/expr.rs`, `expression()` starts at ternary/fat-arrow level and descends:

1. `ternary` (`?? !!`)
2. `or_expr` (`or`)
3. `and_expr` (`and`)
4. `not_expr` (`not`)
5. `or_or_expr` (`||`, `//`)
6. `and_and_expr` (`&&`)
7. `junctive_expr` (`?|`, `?&`, `?^`)
8. `comparison_expr` (`==`, `!=`, `eq`, `~~`, `<=>`, `eqv`, etc.)
9. `range_expr` (`..`, `..^`, `^..`, `^..^`)
10. `structural_expr` (`but`, `does`)
11. `concat_expr` (`~`, `x`)
12. `additive_expr` (`+`, `-`)
13. `multiplicative_expr` (`*`, `/`, `%`, `%%`, `div`, `mod`, `gcd`, `lcm`)
14. `power_expr` (`**`)
15. `prefix_expr` (prefix unary ops)
16. `postfix_expr` (method/index/call/postfix `++`/`--`)
17. `primary` (literals/vars/blocks/calls/etc.)

`expression()` also handles `=>` specially and auto-quotes bareword LHS into a string literal.

## Token/operator model

- Shared token/operator enum: `src/token_kind.rs` (`TokenKind`)
- `TokenKind` is used by parser, AST nodes (`Expr::Unary/Binary/PostfixOp`), compiler, and runtime builtins.
- There is no standalone lexer module anymore; parser logic consumes source text directly.

## Error behavior

- Internal parser functions return `PResult<'a, T> = Result<(&'a str, T), PError>`.
- Top-level `parse_program()` converts parse failures to `RuntimeError` with structured metadata:
  - `code`: `RuntimeErrorCode::{ParseUnparsed, ParseExpected, ParseGeneric}`
  - `line`, `column`: 1-based location for the furthest/near parse position
  - `message`: human-readable summary including a compact `near` snippet
- Unparsed remainder at end of parse is treated as an error.
- `near` snippets and reported columns are aligned (leading whitespace is skipped consistently).

## Change checklist (recommended)

When adding/changing grammar:

1. Decide the layer: statement vs expression vs primary.
2. Add the parser in exactly one responsible file.
3. If it is a statement form, place it carefully in `statement()` order.
4. If it is an operator, place it in the correct precedence function.
5. Reuse/create `TokenKind` variants only when needed by AST/compiler/runtime.
6. Add focused unit tests near parser code (`src/parser_nom/*` tests).
7. Add/adjust prove tests in `t/*.t` for end-to-end behavior.

## Current limitations to keep in mind

- Parsing is currently single-backend (`parser_nom`) and always selected.
- Raku slang switching (main/regex/quote/pod contexts) is only partially modeled; be cautious with context-sensitive constructs.
- Packrat-inspired features are partial: selective memoization exists for `statement`/`expression`/`primary`, and furthest-failure aggregation currently focuses on statement-level alternatives.

## Runtime knobs

- `MUTSU_PARSE_MEMO=0` disables parser memoization caches (useful for A/B debugging and perf comparison).
- `MUTSU_TRACE=parse` prints parser startup and memo stats (`statement`/`expression`/`primary` hit/miss/store counts).

## CLI diagnostics

- `src/main.rs` prints parser/runtime errors via a shared formatter.
- When present, structured metadata is emitted as:
  - `metadata: code=PARSE_..., kind=parse, line=N, column=M`
- This keeps human-readable error text while giving stable fields for tooling.
