# Parser Refactoring Plan

Current state: `src/parser/` is 9,128 lines across 6 files. The three main files (`stmt.rs` 3,626 lines, `primary.rs` 2,732 lines, `expr.rs` 2,164 lines) all exceed the 500-line convention and contain significant duplication.

## 1. Extract memoization into a generic module

**Problem:** Each of `stmt.rs`, `expr.rs`, `primary.rs` contains ~60 lines of nearly identical memoization boilerplate (`*MemoEntry` enum, `*MemoStats` struct, `thread_local!`, `*_memo_key/get/store/reset/stats` functions). Total ~180 lines of copy-paste.

**Plan:**
- Create `src/parser/memo.rs` with a generic `ParseMemo<T: Clone>` struct.
- Provide `get()`, `store()`, `reset()`, `stats()` methods.
- Each call site instantiates `ParseMemo<Expr>` or `ParseMemo<Stmt>` via `thread_local!`.
- Eliminates ~120 lines of duplication.

## 2. Eliminate `_no_seq` expression chain duplication

**Problem:** `expr.rs` lines 869-1078 contain a complete duplicate of the precedence chain (`ternary_no_seq`, `or_expr_no_seq`, `and_expr_no_seq`, `not_expr_no_seq`, `or_or_expr_no_seq`, `and_and_expr_no_seq`, `comparison_expr_no_seq`, `junctive_expr_no_seq`) — ~210 lines that differ from their counterparts only in skipping the sequence operator (`...`) and memoization.

**Plan:**
- Option A: Add a `bool` parameter (or an enum `ExprMode { Full, NoSequence }`) threaded through the chain to control whether sequence parsing is applied.
- Option B: Use a `thread_local!` flag to indicate "no-sequence mode", set before calling the shared chain.
- Option A is cleaner. Each function gains one parameter; the sequence/memo checks become `if mode == Full { ... }`.
- Eliminates ~200 lines.

## 3. Unify string escape-sequence handling

**Problem:** `interpolate_string_content` (line 395, for heredoc content) and `double_quoted_string` (line 609) share ~100 lines of nearly identical backslash escape processing (`\n`, `\t`, `\r`, `\0`, `\x[HH]`, `\xHH`, `\o[OO]`, `\c[NAME]`) and variable/array interpolation logic.

**Plan:**
- Extract a helper `fn process_escape_sequence(rest: &str) -> Option<(char_or_string, &str)>` that handles one backslash escape and returns the result + remaining input.
- Extract `fn try_interpolate_var(rest: &str, parts: &mut Vec<Expr>, current: &mut String) -> Option<&str>` for `$var` and `@var` interpolation.
- Both `interpolate_string_content` and `double_quoted_string` call these helpers in their main loops.
- Eliminates ~80 lines of duplication.

## 4. Deduplicate `is_ident_char`

**Problem:** `fn is_ident_char(b: Option<u8>) -> bool` is defined identically in both `stmt.rs` and `expr.rs`.

**Plan:**
- Move to `helpers.rs` as `pub(super) fn is_ident_char(...)`.
- Both files import from `helpers`.

## 5. Split `stmt.rs` into submodules

**Problem:** `stmt.rs` is 3,626 lines — over 7x the 500-line convention. It contains logically distinct sections that rarely interact.

**Plan:** Split into submodules under `src/parser/stmt/`:
- `stmt/mod.rs` — re-exports, `STMT_PARSERS` dispatch table, `statement()`, `program()`, `stmt_list`, `block`.
- `stmt/decl.rs` — `my_decl`, `has_decl`, `enum_decl`, `constant_decl`, `subset_decl`, `use_stmt`.
- `stmt/control.rs` — `if_stmt`, `given_stmt`, `when_stmt`, `with_stmt`, `for_stmt`, `while_stmt`, `until_stmt`, `loop_stmt`, `repeat_stmt`, `labeled_loop_stmt`.
- `stmt/sub.rs` — `sub_decl`, `method_decl`, `parse_param_list`, `parse_single_param`, `skip_sub_traits`, `skip_return_type_annotation`.
- `stmt/class.rs` — `class_decl`, `role_decl`, `grammar_decl`, `token_decl`, `does_decl`, `proto_decl`, `package_decl`.
- `stmt/assign.rs` — `assign_stmt`, `parse_assign_expr_or_comma`, `try_parse_assign_expr`, `parse_compound_assign_op`, `CompoundAssignOp`.
- `stmt/simple.rs` — `return_stmt`, `last_stmt`, `next_stmt`, `die_stmt`, `take_stmt`, `catch_stmt`, `phaser_stmt`, `known_call_stmt`, etc.
- `stmt/modifier.rs` — `parse_statement_modifier`.
- `stmt/args.rs` — `parse_stmt_call_args`, `parse_stmt_call_args_no_paren`, `parse_remaining_call_args`, `parse_single_call_arg`.

Each submodule stays under 500 lines. `mod.rs` uses `pub(super) use` to maintain the same public API.

## 6. Split `primary.rs` into submodules

**Problem:** `primary.rs` is 2,732 lines — over 5x the convention.

**Plan:** Split into `src/parser/primary/`:
- `primary/mod.rs` — `primary()` dispatch, memoization, `set_original_source`, `current_line_number`.
- `primary/string.rs` — `q_string`, `single_quoted_string`, `double_quoted_string`, `corner_bracket_string`, `interpolate_string_content`, `parse_backslash_c_bracket`, `parse_var_name_from_str` (+ shared escape helpers from item 3).
- `primary/number.rs` — `integer`, `decimal`, `parse_int_radix`.
- `primary/regex.rs` — `regex_lit`, `scan_to_delim`.
- `primary/ident.rs` — `identifier_or_call`, `keyword_literal`, `is_keyword`, `is_listop`, `is_expr_listop`, `parse_expr_listop_args`, `parse_listop_arg`.
- `primary/container.rs` — `paren_expr`, `array_literal`, `angle_list`, `block_or_hash_expr`, `arrow_lambda`.
- `primary/var.rs` — `scalar_var`, `array_var`, `hash_var`, `code_var`, `topic_method_call`.
- `primary/misc.rs` — `whatever`, `class_literal`, `version_lit`, `reduction_op`, `colonpair_expr`, `parse_call_arg_list`.

## 7. Split `expr.rs` into submodules (optional, lower priority)

**Problem:** `expr.rs` is 2,164 lines. After eliminating the `_no_seq` duplication (~200 lines) and extracting memoization (~60 lines), it drops to ~1,900 lines — still large but more manageable.

**Plan (if still too large after items 1-2):** Split into `src/parser/expr/`:
- `expr/mod.rs` — `expression`, `expression_no_sequence`, memoization, WhateverCode logic.
- `expr/operators.rs` — All operator enums (`ComparisonOp`, `ConcatOp`, `AdditiveOp`, etc.) and their recognizer functions (`parse_comparison_op`, `parse_additive_op`, etc.).
- `expr/precedence.rs` — The full precedence chain (`ternary` through `postfix_expr`).

## 8. Extract balanced-delimiter skipper

**Problem:** A `depth: u32` loop tracking `(`/`)` nesting to skip balanced content appears identically in `use_stmt` and `skip_sub_traits`.

**Plan:**
- Add `fn skip_balanced_parens(input: &str) -> &str` to `helpers.rs`.
- Both call sites use the shared helper.

## Priority order

1. **Item 1** (memo) — Mechanical, low risk, immediate deduplication.
2. **Item 4** (`is_ident_char`) — Trivial, do alongside item 1.
3. **Item 2** (`_no_seq`) — Biggest single deduplication win (~200 lines).
4. **Item 3** (string escapes) — Medium effort, clear duplication.
5. **Item 8** (balanced parens) — Trivial.
6. **Item 5** (split `stmt.rs`) — Largest structural change. Do after deduplication so each submodule is smaller.
7. **Item 6** (split `primary.rs`) — Same rationale.
8. **Item 7** (split `expr.rs`) — Only if still needed after items 1-3.

## Expected outcome

- No file exceeds 500 lines.
- No significant code duplication remains.
- Public API of `parser` module unchanged (`parse_program()` only).
- Zero behavioral changes — pure refactoring.
