# Parser Overview (`parser`)

This document summarizes the current hand-written parser so contributors (and AI agents) can make focused changes without re-reading the entire implementation.

For ongoing refactoring policy and roadmap, see `docs/parser-improvement-plan.md`.

## Scope and entry points

- Runtime entry: `src/parse_dispatch.rs` -> `parse_source()`
- Parser entry: `src/parser/mod.rs` -> `parse_program(input)`
- Statement loop: `src/parser/stmt.rs` -> `program()` -> `stmt_list()` -> `statement()`
- Expression entry: `src/parser/expr.rs` -> `expression()`

`parse_program()` also handles `=finish` splitting and returns `(Vec<Stmt>, Option<String>)`.

## File responsibilities

- `src/parser/mod.rs`: top-level parse orchestration and parse error wrapping
- `src/parser/parse_result.rs`: `PResult` / `PError` and tiny parser primitives
- `src/parser/helpers.rs`: whitespace/comment/Pod skipping (`ws`, `ws1`)
- `src/parser/stmt.rs`: statement grammar and statement-level dispatch order
- `src/parser/expr.rs`: operator precedence climbing and expression combinators
- `src/parser/primary.rs`: literals, identifiers/calls, vars, blocks, hashes, regex literals

## Statement dispatch order (important)

`statement()` in `src/parser/stmt.rs` tries many forms in fixed order and returns the first match.
Order changes can alter parse behavior.

High-impact groups in current order:

1. Declarations/modules: `use`, `unit module`, `my`, `constant`, `class/role/grammar/subset/enum/has/does/proto/sub/method/token`
2. Output/conditionals/loops: `say/print/note`, `if/unless/with`, labeled loops, `for/while/until/loop/repeat`, `given/when/default`
3. Control and phasers: `return/last/next/redo/die/take/catch/control/phaser`
4. Reactive/test/package: `subtest/react/whenever/package`
5. Fallbacks: known-call stmt, assignment stmt, block stmt, expression stmt

## Expression precedence (low -> high)

In `src/parser/expr.rs`, `expression()` starts at ternary/fat-arrow level and descends:

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

## User-defined operators

- **Infix.** A `sub infix:<...>` with no precedence trait has **additive** precedence, like rakudo — it is handled by `additive_expr` (`expr/precedence_meta_ops/arith.rs`) via the shared `custom_infix::try_custom_infix_word`, so it binds tighter than `~`, `..`, `&&` and `?? !!`. `is tighter`/`is looser`/`is equiv` route to their own level; only an operator explicitly pushed down to (or below) `PREC_SEQUENCE` reaches the list-infix loop. `parse_custom_infix_word` is deliberately *permissive* (any non-reserved word may be a runtime-installed `my &infix:<...>`), so the additive level is granted only to operators the parser has seen **declared** — an undeclared speculative word keeps the loose last-resort level, or `42 but Str` would parse as `infix:<but>`.
- **Circumfix / postcircumfix.** `declared_circumfix_op` runs early in `primary()` (before the variable parsers). The declared-**post**circumfix check runs in the postfix loop *before* the built-in `(...)`/`[...]`/`{...}` subscripts, under the longest-token rule: a multi-character opener like `[-` beats the built-in `[`, while a one-character opener that is itself a built-in subscript opener ties and the built-in keeps it. A postcircumfix bracket holds ONE argument parsed at comma (list) precedence, not a positional argument list. A registered closing delimiter is never taken as an infix word (`is_circumfix_close_delimiter_word`).

## Quote languages vs declared symbols

Raku's named quote constructs (`Q`, `q`, `qq`, `qw`, `qx`, `m`, `s`, `S`, `tr`, `TR`, `rx`, ...) are spelled as ordinary identifiers, and a **declared** symbol of that name unconditionally removes the quote language spelled that way. `src/parser/quote_shadow.rs` is the single, name-agnostic implementation; every named quote entry point (`big_q_string`, `q_string`, `qx_string`, and `regex_lit` once at the top) calls `quote_lang_shadowed()`. Do NOT add a per-letter guard. The one exception is an explicit adverb (`s:g/…/…/`, `m:i/…/`, `q:w/…/`), which is unambiguously the quote language and wins over any declaration.

## Postcircumfix `{ }` after a term

A `{` glued directly onto a just-parsed term (no intervening whitespace) is
`postcircumfix:<{ }>`, unconditionally — the whitespace is the only thing that
distinguishes a following bare block. Normally the remainder carries that
distinction by itself (with a space it starts with the space, not with `{`), but
several term parsers eat their own trailing whitespace — an inline `my $x`
declaration, the `gather EXPR` statement prefix — and for those the space is
gone by the time the postfix loop looks. So the loop tracks whether the span
consumed for the current term ended on whitespace and feeds that to
`brace_is_postcircumfix` (`expr/postfix/loop_.rs`), which restores the
distinction for every such parser at once.

Do NOT reintroduce a per-`Expr`-shape allow-list here — it used to be one, and
every new term shape that reached this point silently lost its subscript (the
`{...}` became a disconnected block statement, not an error) until someone hit
it and appended the variant by hand. The only shape-based exception left is an
inline `my`/`our`/`state` declaration, which is never subscripted directly in
expression context.

The `{**}` hyperslice, `{||@keys}` dimension splat and the `Type{ ... }`
constructor shorthand are separate arms tried *before* this one and keep their
own narrower conditions.

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
6. Add focused unit tests near parser code (`src/parser/*` tests).
7. Add/adjust prove tests in `t/*.t` for end-to-end behavior.

## Current limitations to keep in mind

- Parsing is currently single-backend (`parser`) and always selected.
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
