mod expr;
// ADR-0033: `crate::whatever_curry::build_closure` (the WhateverCode closure
// construction that moved out of the parser) needs these priming-scope
// predicates, so re-export them at crate visibility without making the whole
// `expr` module (and its many `pub(in crate::parser)`-typed internals) public.
pub(crate) use expr::{contains_whatever, is_whatever, should_wrap_whatevercode};
// `but`-mixing a plain value composes an anonymous role at RUNTIME, and it must
// draw its `<anon|N>` id from the same counter the parser uses for a `role { }`
// literal (see `Interpreter::apply_single_mixin`).
pub(crate) use primary::next_anon_role_name;
pub(crate) mod helpers;
mod memo;
mod outer_redecl;
mod parse_result;
mod primary;
mod quote_shadow;
mod sink_warn;
mod stmt;
pub(crate) mod stmt_ending_brace;
pub(crate) mod term_boundary;
mod whenever_scope;
use std::sync::OnceLock;

/// Prefix of the emitter parameter `supply { … }` lowers to (see
/// `primary::ident::supply::supply_method_call`). It is what identifies a
/// supply-block body after parsing — both for the out-of-scope `whenever` check
/// and for `CompiledCode::is_supply_block_body`.
pub(crate) const SUPPLY_EMITTER_PREFIX: &str = "__mutsu_supply_emitter_";

pub(crate) fn is_imported_function(name: &str) -> bool {
    stmt::simple::is_imported_function(name)
}

/// Parse a quote body under `qq` (double-quote) interpolation rules: variables
/// with their postcircumfixes, embedded `{ ... }` code blocks, and the full
/// backslash-escape set.
///
/// This is the single implementation of that grammar. A construct that Raku
/// specifies as `qq`-quoted but that is not lexically a `"..."` literal — a
/// heredoc body, an `s///` replacement — must route through here rather than
/// grow its own partial interpolator.
pub(crate) fn interpolate_qq_content(content: &str) -> crate::ast::Expr {
    primary::quote_adverbs::process_content_with_flags(
        content,
        &primary::quote_adverbs::QuoteFlags::qq_double(),
    )
}

/// Parse a heredoc body string as an interpolated (qq-style) string expression.
/// Used by the compiler to defer heredoc interpolation to compile time.
pub(crate) fn interpolate_heredoc_content(content: &str) -> crate::ast::Expr {
    interpolate_qq_content(content)
}

/// Slang activation surface for the runtime (ADR-0026): the runtime's
/// `$*LANG.define_slang` maps overridden grammar-rule names onto these
/// parser modes.
pub(crate) use stmt::simple::{apply_slang_rule_override, slang_modes};
pub use stmt::simple::{
    clear_parser_lib_paths, set_parser_lib_paths, set_parser_program_path, set_parser_source_file,
};

pub(crate) use expr::precedence::lower_feed_node;
/// Lower a deferred `Expr::Feed` node into its executable (sink-call) form.
/// Re-exported for the compiler's `Expr::Feed` arm.
pub(crate) use stmt::sub::is_builtin_param_trait;

/// Descend a feed chain to its textually-leftmost operand slot — for splitting a
/// declaration/assignment that binds tighter than the feed.
pub(crate) use expr::precedence::feed_leftmost_operand_mut;

pub(crate) fn current_language_version() -> String {
    stmt::simple::current_language_version()
}

pub(crate) fn set_current_language_version(version: &str) {
    stmt::simple::set_current_language_version(version);
}

/// Run `f` with the given names pre-seeded as known user-sub names, so any parse
/// performed inside `f` (e.g. a re-entrant `Interpreter::run`) recognizes them as
/// callable in listop form. Restores the previous (empty) preseed afterwards.
/// Used by `throws-like`/EVAL so caller-scope lexical `&name` subs parse as calls.
pub(crate) fn with_user_sub_preseed<R>(names: Vec<String>, f: impl FnOnce() -> R) -> R {
    stmt::set_eval_user_sub_preseed(names);
    let result = f();
    stmt::set_eval_user_sub_preseed(Vec::new());
    result
}

use std::cell::RefCell;

use crate::ast::Stmt;
use crate::value::RuntimeErrorCode;
use crate::value::{RuntimeError, Value};

thread_local! {
    /// Collected parse warnings, tagged with the source file being parsed at
    /// the moment each was raised (`parser_source_file()`, see
    /// `add_parse_warning`). The tag lets callers deduplicate a warning that
    /// was surfaced by more than one parse of the same source (e.g. a
    /// module's export scan followed by its run-time load) without
    /// conflating it with an unrelated warning that happens to share the
    /// same message text in a different file.
    static PARSE_WARNINGS: RefCell<Vec<(Option<String>, String)>> =
        const { RefCell::new(Vec::new()) };
    /// 1-based line numbers of detected VCS conflict markers (`<<<<<<<` blocks).
    static VCS_CONFLICT_MARKERS: RefCell<Vec<i64>> = const { RefCell::new(Vec::new()) };
    /// When set, the next `parse_program` call treats the unit's final
    /// statement as a value (return) position, not sink context — the EVAL /
    /// EVALFILE semantics, where the last expression is the EVAL's result.
    /// Consumed (reset to false) by that call, so nested parses (e.g. a `use`
    /// inside the EVAL'd code) fall back to mainline sink semantics.
    static EVAL_VALUE_TAIL: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Arm the value-tail sink semantics for the next `parse_program` call (see
/// `EVAL_VALUE_TAIL`).
///
/// Also used by the REPL: a REPL line's final statement is the line's displayed
/// value, exactly like EVAL's, so `1 + 2 * 3` at the prompt is not sink context
/// and must not warn (rakudo's REPL does not).
pub(crate) fn set_eval_value_tail() {
    EVAL_VALUE_TAIL.with(|f| f.set(true));
}

/// Consume the value-tail flag (one-shot).
fn take_eval_value_tail() -> bool {
    EVAL_VALUE_TAIL.with(|f| f.replace(false))
}

/// Add a warning message during parsing, tagged with `line` (1-based, the
/// caller's own position — see [`primary::current_line_number`]). Collected
/// and emitted after parse completes. Also tags the message with the file
/// currently being parsed (`parser_source_file()`), which is swapped per
/// compilation unit (unlike `parser_program_path()`, which stays pinned to
/// the top-level script) — so a warning raised while parsing a `use`d
/// module's own source correctly names that module's file, not the
/// importer's — see `PARSE_WARNINGS`. Both are baked directly into the
/// stored message text (an `"\n    at FILE:LINE"` suffix, mirroring
/// Rakudo's own compile-warning location line) rather than kept as separate
/// tuple fields, so the location survives the precompilation-cache
/// round-trip for free (`ParseEffects::warnings` only persists the message
/// text — see `take_parse_warnings`'s callers). `write_warn_to_stderr`
/// recognizes this suffix and skips appending its own (wrong, current-
/// execution-position) backtrace.
pub(super) fn add_parse_warning(msg: String, line: i64) {
    let file = stmt::simple::parser_source_file();
    let display_file = file.as_deref().unwrap_or("-e");
    let tagged = format!("{msg}\n    at {display_file}:{line}");
    PARSE_WARNINGS.with(|w| w.borrow_mut().push((file, tagged)));
}

/// Record a detected VCS conflict marker (`<<<<<<<` ... `>>>>>>>` block) at the
/// given 1-based line. Collected during parsing and, if any were found, turned
/// into an `X::Comp::Group` (or a lone `X::Comp::AdHoc`) after the parse — this
/// mirrors rakudo's "Found a version control conflict marker" compile error.
pub(super) fn record_vcs_conflict_marker(line: i64) {
    VCS_CONFLICT_MARKERS.with(|m| m.borrow_mut().push(line));
}

/// Take and clear the collected VCS conflict markers, deduplicating by line
/// (parser backtracking may re-visit the same marker more than once).
fn take_vcs_conflict_markers() -> Vec<i64> {
    let all: Vec<i64> = VCS_CONFLICT_MARKERS.with(|m| m.borrow_mut().drain(..).collect());
    let mut seen = std::collections::HashSet::new();
    let mut lines = Vec::new();
    for line in all {
        if seen.insert(line) {
            lines.push(line);
        }
    }
    lines
}

/// Build the compile-time error rakudo raises for VCS conflict markers. A single
/// marker becomes a bare `X::Comp::AdHoc`; two or more become an
/// `X::Comp::Group` whose `sorrows` hold all but the last marker and whose
/// `panic` is the last one. Each carries `payload`/`message`/`line`.
fn build_vcs_conflict_error(lines: &[i64]) -> RuntimeError {
    const PAYLOAD: &str = "Found a version control conflict marker";
    let make_adhoc = |line: i64| -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("payload".to_string(), Value::str(PAYLOAD.to_string()));
        attrs.insert("message".to_string(), Value::str(PAYLOAD.to_string()));
        attrs.insert("line".to_string(), Value::int(line));
        Value::make_instance(crate::symbol::Symbol::intern("X::Comp::AdHoc"), attrs)
    };

    let mut err = RuntimeError::new(PAYLOAD);
    err.set_code(Some(RuntimeErrorCode::ParseGeneric));
    if lines.len() <= 1 {
        let line = lines.first().copied().unwrap_or(1);
        err.set_line(Some(line as usize));
        err.exception = Some(Box::new(make_adhoc(line)));
        return err;
    }

    let (panic_line, sorrow_lines) = lines.split_last().unwrap();
    let sorrows: Vec<Value> = sorrow_lines.iter().map(|&l| make_adhoc(l)).collect();
    let panic = make_adhoc(*panic_line);
    let mut group_attrs = std::collections::HashMap::new();
    group_attrs.insert("sorrows".to_string(), Value::array(sorrows));
    group_attrs.insert("worries".to_string(), Value::array(vec![]));
    group_attrs.insert("panic".to_string(), panic);
    group_attrs.insert("message".to_string(), Value::str(PAYLOAD.to_string()));
    err.set_line(Some(*panic_line as usize));
    err.exception = Some(Box::new(Value::make_instance(
        crate::symbol::Symbol::intern("X::Comp::Group"),
        group_attrs,
    )));
    err
}

/// Build the compile-time error rakudo raises when a `whenever` block appears
/// outside the lexical scope of a `supply`/`react` block: `X::Comp::WheneverOutOfScope`.
fn build_whenever_out_of_scope_error(line: i64) -> RuntimeError {
    const MESSAGE: &str =
        "Cannot have a 'whenever' block outside the scope of a 'supply' or 'react' block";
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(MESSAGE.to_string()));
    attrs.insert("payload".to_string(), Value::str(MESSAGE.to_string()));
    if line > 0 {
        attrs.insert("line".to_string(), Value::int(line));
    }
    let mut err = RuntimeError::new(MESSAGE);
    err.set_code(Some(RuntimeErrorCode::ParseGeneric));
    if line > 0 {
        err.set_line(Some(line as usize));
    }
    err.exception = Some(Box::new(Value::make_instance(
        crate::symbol::Symbol::intern("X::Comp::WheneverOutOfScope"),
        attrs,
    )));
    err
}

/// Build the compile-time error rakudo raises when a lexical is redeclared with
/// `my`/`state` after it has already been referenced as an outer symbol in the
/// same scope: `X::Redeclaration::Outer`.
fn build_outer_redeclaration_error(symbol: &str, line: i64) -> RuntimeError {
    let message = format!(
        "Lexical symbol '{sym}' is already bound to an outer symbol.  The implicit\n\
         outer binding must be rewritten as 'OUTER::<{sym}>' before you can\n\
         unambiguously declare a new '{sym}' in this scope.",
        sym = symbol
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("payload".to_string(), Value::str(message.clone()));
    attrs.insert("symbol-name".to_string(), Value::str(symbol.to_string()));
    attrs.insert("postfix".to_string(), Value::str(String::new()));
    attrs.insert("what".to_string(), Value::str("symbol".to_string()));
    if line > 0 {
        attrs.insert("line".to_string(), Value::int(line));
    }
    let mut err = RuntimeError::new(&message);
    err.set_code(Some(RuntimeErrorCode::ParseGeneric));
    if line > 0 {
        err.set_line(Some(line as usize));
    }
    err.exception = Some(Box::new(Value::make_instance(
        crate::symbol::Symbol::intern("X::Redeclaration::Outer"),
        attrs,
    )));
    err
}

/// Take and clear all parse warnings collected during the last parse, paired
/// with the source file each was raised against (see `PARSE_WARNINGS`).
/// Deduplicates warnings that may have been generated by parser backtracking.
pub(crate) fn take_parse_warnings() -> Vec<(Option<String>, String)> {
    let all: Vec<(Option<String>, String)> =
        PARSE_WARNINGS.with(|w| w.borrow_mut().drain(..).collect());
    let mut seen = std::collections::HashSet::new();
    let mut warnings = Vec::new();
    for w in all {
        if seen.insert(w.clone()) {
            warnings.push(w);
        }
    }
    warnings
}

static PARSE_MEMO_ENABLED: OnceLock<bool> = OnceLock::new();

pub(super) fn parse_memo_enabled() -> bool {
    *PARSE_MEMO_ENABLED.get_or_init(|| {
        #[cfg(not(target_arch = "wasm32"))]
        {
            std::env::var("MUTSU_PARSE_MEMO")
                .map(|v| v != "0")
                .unwrap_or(true)
        }
        #[cfg(target_arch = "wasm32")]
        {
            true
        }
    })
}

/// Invalidate all parse memo caches.  Called when a new user-declared operator
/// changes parsing behavior (e.g. circumfix, postcircumfix, prefix operators).
pub(crate) fn invalidate_all_memos() {
    if parse_memo_enabled() {
        expr::reset_expression_memo();
        primary::reset_primary_memo();
        stmt::reset_statement_memo();
    }
}

pub(crate) fn angle_word_value(word: &str) -> Value {
    primary::angle_word_value(word)
}

fn line_col_at_offset(source: &str, offset: usize) -> (usize, usize) {
    let offset = offset.min(source.len());
    let prefix = &source[..offset];
    let line = prefix.matches('\n').count() + 1;
    let col = prefix
        .rsplit('\n')
        .next()
        .map(|segment| segment.chars().count() + 1)
        .unwrap_or(1);
    (line, col)
}

fn leading_ws_bytes(input: &str) -> usize {
    input.len().saturating_sub(input.trim_start().len())
}

fn near_snippet(input: &str, max_chars: usize) -> Option<String> {
    let trimmed = input.trim_start();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.chars().take(max_chars).collect())
    }
}

fn parse_error_hint(message: &str) -> Option<&'static str> {
    if message.contains("method name") {
        Some("check method-call syntax after '.' (for example: '$obj.method').")
    } else if message.contains("identifier after '::'") {
        Some("qualified names require an identifier after each '::'.")
    } else if message.contains("after ','") {
        Some("a comma usually requires another expression or argument after it.")
    } else if message.contains("after comparison operator")
        || message.contains("after additive operator")
        || message.contains("after multiplicative operator")
    {
        Some("binary operators require a right-hand expression.")
    } else {
        None
    }
}

fn with_parse_hint(mut err: RuntimeError) -> RuntimeError {
    if let Some(hint) = parse_error_hint(&err.message) {
        err.set_hint(Some(hint.to_string()));
    }
    err
}

/// Parse a full program using the nom-based parser.
/// Returns `(statements, Option<finish_content>)`.
pub(crate) fn parse_program(input: &str) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    // Clear any stale parse warnings from previous/backtracked parses
    PARSE_WARNINGS.with(|w| w.borrow_mut().clear());
    VCS_CONFLICT_MARKERS.with(|m| m.borrow_mut().clear());
    // Consume the EVAL value-tail flag up front so any nested parse this
    // program triggers (module loads, ...) uses plain mainline semantics.
    let eval_value_tail = take_eval_value_tail();
    let memo_enabled = parse_memo_enabled();
    if memo_enabled {
        expr::reset_expression_memo();
        primary::reset_primary_memo();
        stmt::reset_statement_memo();
    }
    // Give this parse its own memo generation so entries keyed to `input` can
    // never be confused with entries a nested parse stored for a since-dropped
    // buffer at the same address (see `memo::MemoKey`).
    let _memo_generation = memo::begin_parse_generation();
    stmt::reset_user_subs();
    crate::trace::trace_log!("parse", "parser start memo={}", memo_enabled);
    primary::set_original_source(input);
    // Split off =finish content before parsing
    let (source, finish_content) = if let Some(idx) = input.find("\n=finish") {
        let content = &input[idx + "\n=finish".len()..];
        // Skip to next newline
        let content = if let Some(nl) = content.find('\n') {
            &content[nl + 1..]
        } else {
            ""
        };
        (&input[..idx], Some(content.to_string()))
    } else {
        (input, None)
    };
    let result = match stmt::program(source) {
        Ok((rest, mut stmts)) => {
            // ADR-0033 Phase 2: classify every `*` leaf as a value
            // (`Expr::Whatever`) or a priming argument (`Expr::WhateverArg`)
            // now that the whole program tree exists. Pure annotation --
            // behaviour-preserving by construction (section 2.2's invariant).
            crate::whatever_curry::mark::mark_program(&mut stmts);
            let rest_trimmed = rest.trim();
            if !rest_trimmed.is_empty() {
                let consumed = source.len() - rest.len();
                let near_offset = consumed + leading_ws_bytes(rest);
                let (line_num, col_num) = line_col_at_offset(source, near_offset);
                let context: String = rest_trimmed.chars().take(60).collect();
                Err(RuntimeError::with_location(
                    format!(
                        "Confused. parse error: unparsed input at line {}, column {}: {:?}",
                        line_num, col_num, context
                    ),
                    RuntimeErrorCode::ParseUnparsed,
                    line_num,
                    col_num,
                ))
            } else if let Some(line) = whenever_scope::find_out_of_scope_whenever(&stmts) {
                // A `whenever` outside a `supply`/`react` block is a compile-time
                // error in rakudo (X::Comp::WheneverOutOfScope).
                Err(build_whenever_out_of_scope_error(line))
            } else if let Some((symbol, line)) = outer_redecl::find_outer_redeclaration(&stmts) {
                // Redeclaring a lexical after referencing its outer binding in the
                // same scope is a compile-time X::Redeclaration::Outer in rakudo.
                Err(build_outer_redeclaration_error(&symbol, line))
            } else {
                if eval_value_tail {
                    sink_warn::add_sink_warnings_value_tail(&stmts);
                } else {
                    sink_warn::add_sink_warnings(&stmts);
                }
                Ok((stmts, finish_content))
            }
        }
        Err(e) => {
            if e.is_fatal() {
                // Fatal parse errors (e.g. bare say/print/put) pass through directly
                let mut err = RuntimeError::new(format!("{}", e));
                err.set_code(Some(RuntimeErrorCode::ParseGeneric));
                // A fatal raised with `fatal_at` carries the failure position;
                // surface it as line/column so the CLI/`is_run` render the
                // ===SORRY!=== snippet with the offending line.
                if let Some(consumed) = e.consumed_from(source.len()) {
                    let tail = &source[consumed..];
                    let near_offset = consumed + leading_ws_bytes(tail);
                    let (line_num, col_num) = line_col_at_offset(source, near_offset);
                    err.set_line(Some(line_num));
                    err.set_column(Some(col_num));
                }
                if let Some(ex) = e.exception {
                    // A fatal diagnosis's own exception (built far from here,
                    // e.g. `pod_begin_without_identifier_error`) usually carries
                    // only `message`. `err.set_line` above computed the real
                    // `line`/`column` from the failure position; without also
                    // copying them onto the exception's own attributes here,
                    // `$!.line`/`$!.column` (the actual X::Comp accessors, read
                    // straight from the instance) stayed unset even though the
                    // CLI's own `===SORRY!===` rendering (which reads `err`
                    // directly) already had them. Other X::Comp builders in this
                    // file (`build_vcs_conflict_error`, etc.) set `line` on their
                    // exception's attrs by hand at construction; this generalizes
                    // that for every site that instead relies on `remaining_len`.
                    if let crate::value::ValueView::Instance { attributes, .. } = ex.view() {
                        if let Some(line) = err.line() {
                            attributes.insert_if_absent(
                                "line".to_string(),
                                crate::value::Value::int(line as i64),
                            );
                        }
                        if let Some(column) = err.column() {
                            attributes.insert_if_absent(
                                "column".to_string(),
                                crate::value::Value::int(column as i64),
                            );
                        }
                    }
                    err.exception = Some(ex);
                }
                return Err(err);
            }
            if let Some(consumed) = e.consumed_from(source.len()) {
                let tail = &source[consumed..];
                let near_offset = consumed + leading_ws_bytes(tail);
                let (line_num, col_num) = line_col_at_offset(source, near_offset);
                // One of the alternatives that failed here may have diagnosed the
                // input precisely and named its Raku exception class in the
                // `"X::Type: text"` convention (`X::Syntax::CannotMeta: Cannot do
                // . because it is too fiddly`). Every message merged into a
                // `PError` shares the same furthest failure position
                // (`update_best_error` only merges at an equal score), so such a
                // message describes *this* failure and is strictly better than
                // the generic "Confused." wrapper — which would otherwise bury it
                // inside an "expected A or B or …" list and leave the exception
                // classed `X::Syntax::Confused`.
                if let Some(typed) = e.typed_convention_message() {
                    let mut err = RuntimeError::with_location(
                        typed.to_string(),
                        RuntimeErrorCode::ParseExpected,
                        line_num,
                        col_num,
                    );
                    // A SOFT diagnosis may still carry a structured exception —
                    // the message convention preserves only the class, and some
                    // sites also need the attributes rakudo's exception has
                    // (`X::UnitScope::Invalid.what`). The fatal branch above
                    // already forwards it; without the same here, a
                    // `throws-like …, X::…, what => …` matched the class and
                    // then died on `No such method 'what'`.
                    if let Some(ex) = e.exception {
                        err.exception = Some(ex);
                    }
                    // rakudo's X::Comp family also carries `.pre`/`.post` (the
                    // source text immediately around the eject point, current
                    // line only). This is the one place both the full original
                    // source and the failure offset are unambiguously known, so
                    // compute it here rather than at each individual raise site.
                    let pre_full = &source[..consumed];
                    let pre = pre_full.rsplit('\n').next().unwrap_or(pre_full).to_string();
                    let post = tail.split('\n').next().unwrap_or(tail).to_string();
                    err.set_pre_post_context(pre, post);
                    Err(with_parse_hint(err))
                } else if let Some(context) = near_snippet(tail, 60) {
                    Err(with_parse_hint(RuntimeError::with_location(
                        format!(
                            "Confused. parse error at line {}, column {}: {} — near: {:?}",
                            line_num, col_num, e, context
                        ),
                        RuntimeErrorCode::ParseExpected,
                        line_num,
                        col_num,
                    )))
                } else {
                    Err(with_parse_hint(RuntimeError::with_location(
                        format!(
                            "Confused. parse error at line {}, column {}: {}",
                            line_num, col_num, e
                        ),
                        RuntimeErrorCode::ParseExpected,
                        line_num,
                        col_num,
                    )))
                }
            } else {
                let mut err = RuntimeError::new(format!("Confused. parse error: {}", e));
                err.set_code(Some(RuntimeErrorCode::ParseGeneric));
                Err(with_parse_hint(err))
            }
        }
    };

    // A VCS conflict marker is a compile-time error and takes precedence over
    // whatever the surrounding statements parsed to (rakudo reports it even when
    // the rest of the unit parses cleanly).
    let conflict_markers = take_vcs_conflict_markers();
    if !conflict_markers.is_empty() {
        return Err(build_vcs_conflict_error(&conflict_markers));
    }

    if memo_enabled && crate::trace::is_enabled("parse") {
        let (stmt_hits, stmt_misses, stmt_stores) = stmt::statement_memo_stats();
        let (expr_hits, expr_misses, expr_stores) = expr::expression_memo_stats();
        let (primary_hits, primary_misses, primary_stores) = primary::primary_memo_stats();
        crate::trace::trace_log!(
            "parse",
            "memo stats stmt[h/m/s]={}/{}/{} expr[h/m/s]={}/{}/{} primary[h/m/s]={}/{}/{}",
            stmt_hits,
            stmt_misses,
            stmt_stores,
            expr_hits,
            expr_misses,
            expr_stores,
            primary_hits,
            primary_misses,
            primary_stores
        );
    }

    result
}

/// Like `parse_program`, but pre-registers operator sub names so the parser
/// recognizes them during EVAL.
pub(crate) fn parse_program_with_operators_and_user_subs(
    input: &str,
    operator_names: &[String],
    operator_assoc: &std::collections::HashMap<String, String>,
    imported_function_names: &[String],
    user_sub_names: &[String],
    user_type_names: &[String],
    user_value_term_names: &[String],
) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    // Set pre-seed operators before calling parse_program.
    // parse_program will call reset_user_subs, then we re-register after.
    stmt::set_eval_operator_preseed(operator_names.to_vec());
    stmt::set_eval_operator_assoc_preseed(operator_assoc.clone());
    stmt::set_eval_imported_function_preseed(imported_function_names.to_vec());
    stmt::set_eval_user_sub_preseed(user_sub_names.to_vec());
    stmt::set_eval_user_type_preseed(user_type_names.to_vec());
    stmt::set_eval_user_value_term_preseed(user_value_term_names.to_vec());
    // EVAL'd code is compiled under the *calling* unit's language revision, not
    // the 6.d default a fresh compilation unit gets (rakudo: `use v6.e.PREVIEW;
    // EVAL 'sprintf("%#x", -256)'` is `-0x100`). Seed the nested parse with it and
    // put ours back afterwards, so a `use vX` inside the EVAL'd string stays
    // lexical to that string.
    let saved_language_version = stmt::simple::current_language_version();
    stmt::set_eval_language_version_preseed(Some(saved_language_version.clone()));
    // Every caller of this entry point evaluates the parsed unit for its value
    // (EVAL / EVAL :check / throws-like code strings), so the final statement
    // is a return position, not sink context.
    set_eval_value_tail();
    let result = parse_program(input);
    stmt::set_eval_operator_preseed(Vec::new());
    stmt::set_eval_operator_assoc_preseed(std::collections::HashMap::new());
    stmt::set_eval_imported_function_preseed(Vec::new());
    stmt::set_eval_user_sub_preseed(Vec::new());
    stmt::set_eval_user_type_preseed(Vec::new());
    stmt::set_eval_user_value_term_preseed(Vec::new());
    stmt::set_eval_language_version_preseed(None);
    stmt::simple::set_current_language_version(&saved_language_version);
    result
}

/// Like `parse_program_partial`, but pre-registers operator sub names so the
/// parser recognizes them during EVAL.  Used to extract BEGIN phasers from
/// code that fails to parse completely.
pub(crate) fn parse_program_partial_with_operators(
    input: &str,
    operator_names: &[String],
    operator_assoc: &std::collections::HashMap<String, String>,
    imported_function_names: &[String],
) -> (Vec<Stmt>, Option<String>) {
    stmt::set_eval_operator_preseed(operator_names.to_vec());
    stmt::set_eval_operator_assoc_preseed(operator_assoc.clone());
    stmt::set_eval_imported_function_preseed(imported_function_names.to_vec());
    stmt::set_eval_user_sub_preseed(Vec::new());
    // Same revision inheritance as `parse_program_with_operators_and_user_subs`:
    // this scans EVAL'd code, so it compiles under the caller's language version.
    stmt::set_eval_language_version_preseed(Some(stmt::simple::current_language_version()));
    let result = parse_program_partial(input);
    stmt::set_eval_operator_preseed(Vec::new());
    stmt::set_eval_operator_assoc_preseed(std::collections::HashMap::new());
    stmt::set_eval_imported_function_preseed(Vec::new());
    stmt::set_eval_user_sub_preseed(Vec::new());
    stmt::set_eval_language_version_preseed(None);
    result
}

/// Best-effort parse: returns all statements that could be parsed before the
/// first error.  Used for loading `.rakumod` modules that may contain syntax
/// mutsu does not yet support.
pub(crate) fn parse_program_partial(input: &str) -> (Vec<Stmt>, Option<String>) {
    let memo_enabled = parse_memo_enabled();
    if memo_enabled {
        expr::reset_expression_memo();
        primary::reset_primary_memo();
        stmt::reset_statement_memo();
    }
    // Fresh memo generation for this nested parse of a (usually short-lived)
    // buffer; the guard restores the enclosing parse's generation on return so
    // the entries this parse stored can never leak into the outer parse via
    // allocator address reuse (see `memo::MemoKey`).
    let _memo_generation = memo::begin_parse_generation();
    // This is a best-effort nested sub-parse (module export scan / EVAL / pseudo
    // package). It must not leak the scanned source's `use vX` pragma into the
    // caller: `reset_user_subs` resets the language version to the 6.d default and
    // the nested parse then adopts whatever pragma `input` declares, so without a
    // restore a `use SomeModule` in a `use v6.e.PREVIEW` program silently dropped
    // the caller back to 6.d for every later version-gated behavior (sprintf flag
    // semantics, submethod dispatch, ...). Snapshot before the reset.
    let saved_language_version = stmt::simple::current_language_version();
    stmt::reset_user_subs();
    // It must not leave `ORIGINAL_SOURCE` pointing at `input` either — once
    // `input` (often a temporary module String) is dropped, the enclosing parse's
    // `current_line_number` would fall back to 1 for every statement. Snapshot the
    // caller's source state and restore it before returning.
    let saved_source_state = primary::snapshot_source_state();
    primary::set_original_source(input);
    let (source, finish_content) = if let Some(idx) = input.find("\n=finish") {
        let content = &input[idx + "\n=finish".len()..];
        let content = if let Some(nl) = content.find('\n') {
            &content[nl + 1..]
        } else {
            ""
        };
        (&input[..idx], Some(content.to_string()))
    } else {
        (input, None)
    };
    let (stmts, _) = stmt::stmt_list_partial(source);
    primary::restore_source_state(saved_source_state);
    stmt::simple::set_current_language_version(&saved_language_version);
    (stmts, finish_content)
}

#[cfg(test)]
mod tests {
    use super::parse_program;
    use crate::ast::{Expr, Stmt};
    use crate::value::{RuntimeErrorCode, ValueView};

    /// Filter out SetLine statements from parsed output for test assertions.
    fn filter_setline(stmts: Vec<Stmt>) -> Vec<Stmt> {
        stmts
            .into_iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect()
    }

    #[test]
    fn parse_program_reports_line_and_column_for_unparsed_input() {
        let err = parse_program("}").unwrap_err();
        assert!(err.message.contains("line 1, column 1"));
        assert!(err.message.contains("unparsed input"));
        assert!(matches!(err.code(), Some(RuntimeErrorCode::ParseUnparsed)));
        assert_eq!(err.line(), Some(1));
        assert_eq!(err.column(), Some(1));
    }

    #[test]
    fn parse_program_reports_line_and_column_for_parse_error() {
        // `ok(,)` used to be the input here, but a comma in term position now
        // has its own diagnosis (`X::Syntax::InfixInTermPosition`, pinned by
        // `parse_program_reports_infix_in_term_position` below), so this case
        // needs input that genuinely has no better description than "confused".
        let err = parse_program("say 1;\nsay 1 ]").unwrap_err();
        assert!(err.message.contains("line 2"));
        assert!(err.message.contains("column"));
        assert!(err.message.contains("parse error"));
        assert!(matches!(err.code(), Some(RuntimeErrorCode::ParseExpected)));
        assert_eq!(err.line(), Some(2));
    }

    #[test]
    fn parse_program_reports_infix_in_term_position() {
        let err = parse_program("say 1;\nok(,)").unwrap_err();
        assert!(
            err.message
                .contains("Preceding context expects a term, but found infix , instead."),
            "{}",
            err.message
        );
        assert_eq!(err.line(), Some(2));
    }

    #[test]
    fn parse_program_unparsed_column_skips_leading_whitespace() {
        let err = parse_program("say 1;\n   }").unwrap_err();
        assert!(err.message.contains("line 2, column 4"));
    }

    #[test]
    fn parse_program_includes_hint_for_common_method_error() {
        let err = parse_program("$x.").unwrap_err();
        assert!(err.message.contains("parse error"));
        assert!(
            err.hint()
                .is_some_and(|hint| hint.contains("method-call syntax"))
        );
    }

    #[test]
    fn parse_program_accepts_corner_bracket_string_in_listop_call() {
        let src = "sub f($a, $b, $c) { }\nf ｢say 42｣, {:out(\"ok\")}, 'msg';";
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        assert_eq!(stmts.len(), 2);
        match &stmts[1] {
            Stmt::Expr(Expr::Call { name, args }) => {
                assert_eq!(name, "f");
                assert_eq!(args.len(), 3);
                assert!(
                    matches!(&args[0], Expr::Literal(v) if matches!(v.view(), ValueView::Str(s) if s.as_str() == "say 42"))
                );
                assert!(matches!(&args[1], Expr::Hash(_)));
                assert!(
                    matches!(&args[2], Expr::Literal(v) if matches!(v.view(), ValueView::Str(s) if s.as_str() == "msg"))
                );
            }
            other => panic!("expected function call expression, got {other:?}"),
        }
    }

    #[test]
    fn parse_program_accepts_french_quote_word_list() {
        let src = "my @target = $*DISTRO.is-win ?? «/c \"\"» !! '/dev/null';";
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn parse_program_accepts_double_angle_quote_word_list_with_quoted_word() {
        let src = "my @str = <<do gjump sover \"\\r\\nth\" elaz yfo x>>;";
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        assert_eq!(stmts.len(), 1);
        let Stmt::VarDecl { expr, .. } = &stmts[0] else {
            panic!("expected VarDecl")
        };
        let items = match expr {
            Expr::ArrayLiteral(items) => items,
            Expr::Call { name, args } if name.resolve() == "list" => args,
            _ => panic!("expected list expression"),
        };
        assert_eq!(items.len(), 7);
        assert!(matches!(
            &items[3],
            Expr::Literal(v) if matches!(v.view(), ValueView::Str(s) if s.as_str() == "\r\nth")
        ));
    }

    #[test]
    fn parse_program_accepts_french_quote_word_list_with_interpolation() {
        let src =
            r#"subtest 'x' => { with Proc::Async.new: «"$*EXECUTABLE" -e "print 'ok'"» { } };"#;
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn parse_program_does_not_bind_with_statement_across_newline_as_modifier() {
        let src = r#"
{
    my $proc = Proc::Async.new($*EXECUTABLE, '-e', '$*OUT.write(Blob.new(65, 66, 67, 13, 10))');
    my $result = '';
    $proc.stdout.tap({ $result ~= $_ });
    await $proc.start;
}
with Proc::Async.new: :out, ($*EXECUTABLE, '-e'), 'say "pass"' {
    .stdout.tap: { }
}
"#;
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn parse_program_accepts_unicode_single_quoted_regex_atoms() {
        let src = r#"
	ok("ab/cd" ~~ m/ab ‘/’ c d/, "curly single quote");
	ok("ab/cd" ~~ m/ab ‚/’ c d/, "low-high single quote");
	ok("ab/cd" ~~ m/ab ‚/’ c d/, "low-curly single quote");
	ok("ab/cd" ~~ m/ab ｢/｣ c d/, "corner quote");
	"#;
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        assert_eq!(stmts.len(), 4);
    }

    #[test]
    fn parse_program_accepts_unicode_and_ascii_minus_angle_complex_in_is_deeply() {
        let src = "use Test;\nis-deeply −<42+2i>, -<42+2i>, 'prefix, Complex';";
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        assert_eq!(stmts.len(), 2);
    }

    #[test]
    fn parse_program_accepts_test_call_with_bracket_metaop_assign_argument() {
        let src = r#"use Test; my $y = 5; is $y [R/]= 1, 1/5, "[R/]= works correctly (1)";"#;
        let parsed = parse_program(src);
        assert!(parsed.is_ok(), "{parsed:?}");
    }

    #[test]
    fn parse_program_accepts_reverse_roast_block() {
        let src = r#"
use Test;
{
    my $y = 5;
    is $y [R/]= 1, 1/5, '[R/]= works correctly (1)';
    sub r2cf(Rat $x is copy) {
        gather $x [R/]= 1 while ($x -= take $x.floor) > 0
    }
}
"#;
        let parsed = parse_program(src);
        assert!(parsed.is_ok(), "{parsed:?}");
    }

    #[test]
    fn parse_program_accepts_q_to_quoted_delim_inside_call_then_method() {
        let src = r#"
my @precompiled = Test::Util::run( "use lib x\n" ~ q:to"--END--").lines;
    for <C A B> {
        say 1;
    }
    --END--
say @precompiled.elems;
"#;
        let parsed = parse_program(src);
        assert!(parsed.is_ok(), "{parsed:?}");
    }

    #[test]
    fn parse_program_accepts_listop_q_concat_argument_for_user_sub() {
        let src = r#"
sub is_run($a, $b, $c) { }
is_run q<use lib '> ~ $pkg-path ~ q<'; use GH2897-B; (^3).map( { my-counter } ).join(",").print>,
       { :err(''), :out('0,1,2'), :status => 0 },
       'closure is preserved after deserialzation';
"#;
        let parsed = parse_program(src);
        assert!(parsed.is_ok(), "{parsed:?}");
    }

    #[test]
    fn parse_program_accepts_named_sub_literal_in_statement_modifier_for_argument() {
        let src = r#"use Test; is ((sub r { "OH HAI" })() for 5), "OH HAI";"#;
        let parsed = parse_program(src);
        assert!(parsed.is_ok(), "{parsed:?}");
    }

    #[test]
    fn parse_program_accepts_named_sub_literal_with_traits_in_expression_context() {
        let src = r#"my &f = sub named is rw { 42 }; say f();"#;
        let parsed = parse_program(src);
        assert!(parsed.is_ok(), "{parsed:?}");
    }

    #[test]
    fn parse_program_accepts_postfix_for_after_hash_index_default_assign() {
        let src = r#"my @a = <a b c>; my %h; %h{.value} //= .key for @a.pairs;"#;
        let parsed = parse_program(src);
        assert!(parsed.is_ok(), "{parsed:?}");
    }

    #[test]
    fn parse_program_rejects_explicit_do_block_with_postfix_for() {
        let src = r#"my $i; do { $i++ } for 1..3;"#;
        let err = parse_program(src).expect_err("expected do...for parse error");
        // The class lives in the attached exception object, not in the message
        // text — see `PError::obsolete`.
        let class = match err.exception.as_ref().map(|ex| ex.view()) {
            Some(crate::value::ValueView::Instance { class_name, .. }) => class_name.to_string(),
            _ => String::new(),
        };
        assert_eq!(class, "X::Obsolete", "{err:?}");
    }

    #[test]
    fn parse_program_accepts_leading_dot_decimal_with_postfix_and_method_in_listop_args() {
        let src = r#"use Test; sub postfix:<R>($x) { $x.FatRat }; isa-ok .88888888888R.WHAT, FatRat, 'leading-dot decimal with postfix/method in args';"#;
        let parsed = parse_program(src);
        assert!(parsed.is_ok(), "{parsed:?}");
    }

    #[test]
    fn parse_program_stops_user_sub_args_before_loose_and() {
        let src = r#"sub isfive(*@args) { }; isfive 5 and isfive 5;"#;
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        match &stmts[1] {
            Stmt::Expr(Expr::Binary { left, op, right }) => {
                assert_eq!(*op, crate::token_kind::TokenKind::AndWord);
                match left.as_ref() {
                    Expr::Call { name, args } => {
                        assert_eq!(name.resolve(), "isfive");
                        assert_eq!(args.len(), 1);
                        assert!(
                            matches!(&args[0], Expr::Literal(v) if matches!(v.view(), ValueView::Int(5)))
                        );
                    }
                    other => panic!("expected lhs call, got {other:?}"),
                }
                match right.as_ref() {
                    Expr::Call { name, args } => {
                        assert_eq!(name.resolve(), "isfive");
                        assert_eq!(args.len(), 1);
                        assert!(
                            matches!(&args[0], Expr::Literal(v) if matches!(v.view(), ValueView::Int(5)))
                        );
                    }
                    other => panic!("expected rhs call, got {other:?}"),
                }
            }
            other => panic!("expected binary and expression, got {other:?}"),
        }
    }

    #[test]
    fn parse_program_keeps_comparison_inside_user_sub_arg() {
        let src = r#"sub foo($x) { }; foo 3 != 3;"#;
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        match &stmts[1] {
            Stmt::Expr(Expr::Call { name, args }) => {
                assert_eq!(name.resolve(), "foo");
                assert_eq!(args.len(), 1);
                assert!(matches!(
                    args[0],
                    Expr::Binary {
                        op: crate::token_kind::TokenKind::BangEq,
                        ..
                    }
                ));
            }
            other => panic!("expected foo call, got {other:?}"),
        }
    }

    #[test]
    fn parse_program_keeps_eq_inside_named_unary_arg() {
        let src = r#"uc "a" eq "A";"#;
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        match &stmts[0] {
            Stmt::Expr(Expr::Call { name, args }) => {
                assert_eq!(name.resolve(), "uc");
                assert_eq!(args.len(), 1);
                assert!(matches!(
                    args[0],
                    Expr::Binary {
                        op: crate::token_kind::TokenKind::Ident(ref op),
                        ..
                    } if op == "eq"
                ));
            }
            other => panic!("expected uc call, got {other:?}"),
        }
    }

    #[test]
    fn parse_program_imports_inline_operator_precedence_metadata() {
        let src = r#"
module RT128042 {
    multi infix:<§>($,$) is tighter(&[+]) is export { 0 };
}
import RT128042;
is (1 + 2 § 3), 1, "x";
"#;
        let (stmts, _) = parse_program(src).unwrap();
        let stmts = filter_setline(stmts);
        match &stmts[2] {
            Stmt::Expr(Expr::Call { name, args }) => {
                assert_eq!(name.resolve(), "is");
                match &args[0] {
                    Expr::Binary { left, op, right } => {
                        assert!(
                            matches!(left.as_ref(), Expr::Literal(v) if matches!(v.view(), ValueView::Int(1)))
                        );
                        assert_eq!(*op, crate::token_kind::TokenKind::Plus);
                        assert!(matches!(
                            right.as_ref(),
                            Expr::InfixFunc { name, .. } if name == "§"
                        ));
                    }
                    other => panic!("expected additive expression, got {other:?}"),
                }
            }
            other => panic!("expected is call, got {other:?}"),
        }
    }
}
