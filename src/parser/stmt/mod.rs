mod args;
pub(super) mod assign;
pub(super) mod class;
mod control;
pub(crate) mod decl;
pub(super) mod modifier;
pub(super) mod simple;
mod sub;

use super::memo::{MemoEntry, MemoStats, ParseMemo};
use super::parse_result::{PError, PResult, parse_char, take_while1, update_best_error};
use std::cell::RefCell;
use std::collections::HashMap;

use crate::ast::Stmt;

use super::helpers::{is_ident_char, ws};

// Re-export submodule items used across submodules
use args::{parse_stmt_call_args, parse_stmt_call_args_no_paren};
use assign::{assign_stmt, parse_assign_expr_or_comma, parse_comma_or_expr, try_parse_assign_expr};
use class::class_decl_body;
use modifier::{is_stmt_modifier_keyword, parse_statement_modifier};
use sub::{method_decl_body, parse_param_list, parse_sub_traits, sub_decl_body};

thread_local! {
    static STMT_MEMO_TLS: RefCell<HashMap<(usize, usize), MemoEntry<Stmt>>> = RefCell::new(HashMap::new());
    static STMT_MEMO_STATS_TLS: RefCell<MemoStats> = RefCell::new(MemoStats::default());
}

static STMT_MEMO: ParseMemo<Stmt> = ParseMemo::new(&STMT_MEMO_TLS, &STMT_MEMO_STATS_TLS);

pub(super) fn reset_statement_memo() {
    STMT_MEMO.reset();
}

pub(super) fn reset_user_subs() {
    simple::reset_user_subs();
}

pub(super) fn statement_memo_stats() -> (usize, usize, usize) {
    STMT_MEMO.stats()
}

/// Try to match a keyword at the start of input, ensuring word boundary.
pub(super) fn keyword<'a>(kw: &str, input: &'a str) -> Option<&'a str> {
    if input.starts_with(kw) && !is_ident_char(input.as_bytes().get(kw.len()).copied()) {
        Some(&input[kw.len()..])
    } else {
        None
    }
}

/// Parse an identifier (alphanumeric, _, -).
/// Hyphen is only allowed when followed by an alphabetic char (not a digit).
fn ident(input: &str) -> PResult<'_, String> {
    let (rest, name) = parse_raku_ident(input)?;
    Ok((rest, name.to_string()))
}

/// Parse a Raku-style identifier.
/// Allows hyphens and apostrophes between word segments, but only when followed by a letter.
/// e.g. `foo-bar` is valid, `doesn't` is valid, but `foo-3` stops at `foo`.
pub(super) fn parse_raku_ident<'a>(input: &'a str) -> PResult<'a, &'a str> {
    // Check first character: must be alphanumeric (including Unicode) or underscore
    let first = input
        .chars()
        .next()
        .ok_or_else(|| PError::expected("identifier"))?;
    if !first.is_alphanumeric() && first != '_' {
        return Err(PError::expected("identifier"));
    }
    let mut end = first.len_utf8();
    // Continue consuming identifier characters
    let mut chars = input[end..].chars().peekable();
    while let Some(&c) = chars.peek() {
        if c.is_alphanumeric() || c == '_' {
            end += c.len_utf8();
            chars.next();
        } else if c == '-' || c == '\'' {
            // Hyphen/apostrophe is part of identifier only if followed by a letter
            let mut lookahead = chars.clone();
            lookahead.next(); // skip the hyphen/apostrophe
            if let Some(&next) = lookahead.peek() {
                if next.is_alphabetic() {
                    end += 1; // consume the hyphen/apostrophe
                    chars.next();
                } else {
                    break;
                }
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok((&input[end..], &input[..end]))
}

/// Parse a qualified identifier (Foo::Bar::Baz).
fn qualified_ident(input: &str) -> PResult<'_, String> {
    let (mut rest, name) = ident(input)?;
    let mut full = name;
    while rest.starts_with("::") {
        let r = &rest[2..];
        // Handle ::<SYMBOL> subscript syntax (e.g., CORE::<&run>)
        if let Some(after_bracket) = r.strip_prefix('<')
            && let Some(end) = after_bracket.find('>')
        {
            let symbol = &after_bracket[..end];
            full.push_str("::");
            full.push_str(symbol);
            rest = &after_bracket[end + 1..];
            continue;
        }
        if let Ok((r2, part)) = ident(r) {
            full.push_str("::");
            full.push_str(&part);
            rest = r2;
        } else {
            return Err(PError::expected_at("identifier after '::'", r));
        }
    }
    Ok((rest, full))
}

/// Parse a variable name ($x, @arr, %hash) and return just the name part.
fn var_name(input: &str) -> PResult<'_, String> {
    if input.starts_with('$')
        || input.starts_with('@')
        || input.starts_with('%')
        || input.starts_with('&')
    {
        let r = &input[1..];
        // Handle twigils
        let (r, twigil) =
            if r.starts_with('*') || r.starts_with('?') || r.starts_with('!') || r.starts_with('^')
            {
                (&r[1..], &r[..1])
            } else {
                (r, "")
            };
        // Handle $_ special
        if input.starts_with('$')
            && r.starts_with('_')
            && (r.len() == 1
                || !r
                    .as_bytes()
                    .get(1)
                    .is_some_and(|c| c.is_ascii_alphanumeric()))
        {
            return Ok((&r[1..], "_".to_string()));
        }
        // Handle $/ special
        if input.starts_with('$') && r.starts_with('/') {
            return Ok((&r[1..], "/".to_string()));
        }
        // Handle bare $ (anonymous variable) — no name after sigil
        if let Ok((rest, name)) =
            take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
        {
            let full = if twigil.is_empty() {
                name.to_string()
            } else {
                format!("{}{}", twigil, name)
            };
            Ok((rest, full))
        } else if input.starts_with('$') && twigil.is_empty() {
            // Anonymous scalar variable: bare $
            Ok((r, "__ANON_STATE__".to_string()))
        } else if input.starts_with('@') && twigil.is_empty() {
            Ok((r, "__ANON_ARRAY__".to_string()))
        } else if input.starts_with('%') && twigil.is_empty() {
            Ok((r, "__ANON_HASH__".to_string()))
        } else {
            Err(PError::expected("variable name"))
        }
    } else {
        Err(PError::expected("variable name"))
    }
}

/// Parse a block: { stmts }
/// Pushes/pops a lexical import scope so that `use` inside a block
/// only affects that block and its children.
fn block(input: &str) -> PResult<'_, Vec<Stmt>> {
    let (input, _) = parse_char(input, '{')?;
    simple::push_scope();
    let result = block_inner(input);
    simple::pop_scope();
    result
}

fn block_inner(input: &str) -> PResult<'_, Vec<Stmt>> {
    let (input, stmts) = stmt_list(input)?;
    let (input, _) = ws(input)?;
    let (input, _) = parse_char(input, '}')?;
    Ok((input, stmts))
}

/// Public accessor for stmt_list (used by primary.rs for block expressions).
pub(super) fn stmt_list_pub(input: &str) -> PResult<'_, Vec<Stmt>> {
    stmt_list(input)
}

/// Public accessor for ident (used by primary.rs for hash literal detection).
pub(super) fn ident_pub(input: &str) -> PResult<'_, String> {
    ident(input)
}

/// Public accessor for var_name (used by primary.rs for anon sub params).
pub(super) fn var_name_pub(input: &str) -> PResult<'_, String> {
    var_name(input)
}

/// Public accessor for parse_param_list (used by primary.rs for arrow lambda sub-signatures).
pub(super) fn parse_param_list_pub(input: &str) -> PResult<'_, Vec<crate::ast::ParamDef>> {
    parse_param_list(input)
}

/// Public accessor for constant declaration parser (used by primary.rs in expression context).
pub(super) fn constant_decl_pub(input: &str) -> PResult<'_, Stmt> {
    decl::constant_decl(input)
}

/// Public accessor for statement (used by primary.rs for do-statement expressions).
pub(super) fn statement_pub(input: &str) -> PResult<'_, Stmt> {
    statement(input)
}

/// Parse `my`/`our`/`state` in expression context — does not consume
/// semicolons or apply statement modifiers.
pub(super) fn my_decl_expr_pub(input: &str) -> PResult<'_, Stmt> {
    decl::my_decl_expr(input)
}

/// Public accessor for `for` statement parser (used by primary.rs for `for` expressions).
pub(super) fn for_stmt_pub(input: &str) -> PResult<'_, Stmt> {
    control::for_stmt(input)
}

pub(super) fn labeled_loop_stmt_pub(input: &str) -> PResult<'_, Stmt> {
    control::labeled_loop_stmt(input)
}

/// Parse statements in best-effort mode: return all successfully parsed
/// statements even if a parse error is encountered partway through.
/// When a statement fails to parse, skip forward to the next statement
/// boundary and continue parsing. Used by `load_module` so that
/// partially-parseable `.rakumod` files still export their functions.
pub(super) fn stmt_list_partial(input: &str) -> (Vec<Stmt>, Option<String>) {
    let mut stmts = Vec::new();
    let mut rest = input;
    while let Ok((r, _)) = ws(rest) {
        let r = consume_semicolons(r);
        let Ok((r, _)) = ws(r) else { break };
        if r.is_empty() || r.starts_with('}') {
            break;
        }
        match statement(r) {
            Ok((r, stmt)) => {
                stmts.push(stmt);
                rest = r;
            }
            Err(_) => {
                // Skip to next statement boundary and continue.
                match skip_to_next_statement(r) {
                    Some(next) => rest = next,
                    None => break,
                }
            }
        }
    }
    (stmts, None)
}

/// Advance past the current unparseable statement by tracking brace/paren
/// depth and stopping after a `;` or `}` at depth-0 that likely marks the
/// end of the current statement.
fn skip_to_next_statement(input: &str) -> Option<&str> {
    let bytes = input.as_bytes();
    let len = bytes.len();
    let mut brace_depth: i32 = 0;
    let mut paren_depth: i32 = 0;
    let mut in_single_quote = false;
    let mut in_double_quote = false;
    let mut prev_was_backslash = false;
    let mut i = 0;
    while i < len {
        let c = bytes[i] as char;
        if prev_was_backslash {
            prev_was_backslash = false;
            i += 1;
            continue;
        }
        if c == '\\' && (in_single_quote || in_double_quote) {
            prev_was_backslash = true;
            i += 1;
            continue;
        }
        // Only treat ' as quote delimiter if it's not part of an identifier
        // (e.g., "doesn't-hang" should not toggle quoting)
        if c == '\'' && !in_double_quote {
            let is_identifier_apostrophe = if i > 0 {
                let prev = bytes[i - 1];
                prev.is_ascii_alphanumeric() || prev == b'-' || prev == b'_'
            } else {
                false
            } && if i + 1 < len {
                let next = bytes[i + 1];
                next.is_ascii_alphanumeric() || next == b'-' || next == b'_'
            } else {
                false
            };
            if !is_identifier_apostrophe {
                in_single_quote = !in_single_quote;
                i += 1;
                continue;
            }
        }
        if c == '"' && !in_single_quote {
            in_double_quote = !in_double_quote;
            i += 1;
            continue;
        }
        if in_single_quote || in_double_quote {
            i += 1;
            continue;
        }
        match c {
            '{' => brace_depth += 1,
            '}' => {
                if brace_depth > 0 {
                    brace_depth -= 1;
                }
                if brace_depth == 0 && paren_depth == 0 {
                    // End of a block — skip past it
                    let after = &input[i + 1..];
                    let after = after.trim_start();
                    // Consume optional trailing semicolons
                    return Some(consume_semicolons(after));
                }
            }
            '(' => paren_depth += 1,
            ')' => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                }
            }
            ';' if brace_depth == 0 && paren_depth == 0 => {
                return Some(&input[i + 1..]);
            }
            _ => {}
        }
        i += 1;
    }
    None
}

/// Parse a list of statements (inside a block or at program level).
fn stmt_list(input: &str) -> PResult<'_, Vec<Stmt>> {
    let mut stmts = Vec::new();
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        // Consume any standalone semicolons
        let r = consume_semicolons(r);
        let (r, _) = ws(r)?;
        // End of block or input
        if r.is_empty() || r.starts_with('}') {
            return Ok((r, stmts));
        }
        match statement(r) {
            Ok((r, stmt)) => {
                stmts.push(stmt);
                rest = r;
            }
            Err(e) => {
                if e.is_fatal() {
                    return Err(e);
                }
                let consumed = input.len() - r.len();
                let line_num = input[..consumed].matches('\n').count() + 1;
                let context: String = r.chars().take(80).collect();
                return Err(PError::raw(
                    format!(
                        "expected statement at line {} (after {} stmts): {} — near: {:?}",
                        line_num,
                        stmts.len(),
                        e,
                        context
                    ),
                    Some(r.len()),
                ));
            }
        }
    }
}

/// Consume zero or more semicolons.
fn consume_semicolons(mut input: &str) -> &str {
    while input.starts_with(';') {
        input = &input[1..];
        // Also consume whitespace after semicolons
        if let Ok((r, _)) = ws(input) {
            input = r;
        }
    }
    input
}

pub(super) fn parse_pointy_param_pub(input: &str) -> PResult<'_, String> {
    control::parse_pointy_param(input)
}

/// Statement parser function type.
type StmtParser = fn(&str) -> PResult<'_, Stmt>;

/// Dispatch table for statement parsers.
/// Each parser is tried in order until one succeeds.
/// Order is critical — do not reorder without careful consideration.
const STMT_PARSERS: &[StmtParser] = &[
    decl::use_stmt,
    class::unit_module_stmt,
    decl::my_decl,
    decl::constant_decl,
    class::class_decl,
    class::role_decl,
    class::grammar_decl,
    decl::subset_decl,
    decl::anon_enum_decl,
    decl::enum_decl,
    decl::has_decl,
    class::does_decl,
    class::proto_decl,
    sub::sub_decl,
    sub::method_decl,
    class::token_decl,
    simple::say_stmt,
    simple::put_stmt,
    simple::print_stmt,
    simple::note_stmt,
    control::if_stmt,
    control::unless_stmt,
    control::with_stmt,
    control::labeled_loop_stmt,
    control::for_stmt,
    control::while_stmt,
    control::until_stmt,
    control::loop_stmt,
    control::repeat_stmt,
    control::given_stmt,
    control::when_stmt,
    control::default_stmt,
    simple::return_stmt,
    simple::last_stmt,
    simple::next_stmt,
    simple::redo_stmt,
    simple::die_stmt,
    simple::take_stmt,
    simple::catch_stmt,
    simple::control_stmt,
    simple::phaser_stmt,
    simple::subtest_stmt,
    control::react_stmt,
    control::whenever_stmt,
    class::package_decl,
    simple::let_stmt,
    simple::temp_stmt,
    simple::known_call_stmt,
    assign_stmt,
    simple::block_stmt,
];

fn statement(input: &str) -> PResult<'_, Stmt> {
    let (input, _) = ws(input)?;
    if let Some(cached) = STMT_MEMO.get(input) {
        return cached;
    }
    let input_len = input.len();
    let mut best_error: Option<(usize, PError)> = None;
    let mut early_success: Option<(&str, Stmt)> = None;

    // Try each statement parser in order
    for parser in STMT_PARSERS {
        match parser(input) {
            Ok(r) => {
                early_success = Some(r);
                break;
            }
            Err(err) => {
                if err.is_fatal() {
                    let result = Err(err);
                    STMT_MEMO.store(input, &result);
                    return result;
                }
                update_best_error(&mut best_error, err, input_len);
            }
        }
    }

    let result = if let Some(r) = early_success {
        Ok(r)
    } else {
        // Fall back to expression statement
        match simple::expr_stmt(input) {
            Ok(r) => Ok(r),
            Err(err) => {
                update_best_error(&mut best_error, err, input_len);
                Err(best_error
                    .map(|(_, err)| err)
                    .unwrap_or_else(|| PError::expected_at("statement", input)))
            }
        }
    };
    STMT_MEMO.store(input, &result);
    result
}

/// Parse a full program (sequence of statements).
pub(super) fn program(input: &str) -> PResult<'_, Vec<Stmt>> {
    // Strip BOM if present
    let input = if let Some(stripped) = input.strip_prefix('\u{FEFF}') {
        stripped
    } else {
        input
    };
    stmt_list(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{CallArg, Expr};
    use crate::value::Value;

    #[test]
    fn parse_use_test() {
        let (rest, stmts) = program("use Test;").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::Use { module, .. } if module == "Test"));
    }

    #[test]
    fn parse_plan_call() {
        let (rest, stmts) = program("use Test;\nplan 1;").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 2);
        assert!(matches!(&stmts[1], Stmt::Call { name, .. } if name == "plan"));
    }

    #[test]
    fn parse_ok_call_with_named() {
        let (rest, stmts) = program("use Test;\nok 0, :todo(1);").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 2);
        if let Stmt::Call { name, args } = &stmts[1] {
            assert_eq!(name, "ok");
            assert_eq!(args.len(), 2);
            assert!(matches!(&args[1], CallArg::Named { name, .. } if name == "todo"));
        } else {
            panic!("Expected Call");
        }
    }

    #[test]
    fn parse_my_var_decl() {
        let (rest, stmts) = program("my $x = '0';").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::VarDecl { name, .. } if name == "x"));
    }

    #[test]
    fn parse_plan_skip_all() {
        let (rest, stmts) = program("use Test;\nplan skip-all => \"msg\";").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 2);
        if let Stmt::Call { name, args } = &stmts[1] {
            assert_eq!(name, "plan");
            assert!(matches!(&args[0], CallArg::Named { name, .. } if name == "skip-all"));
        } else {
            panic!("Expected Call");
        }
    }

    #[test]
    fn parse_basic_test_program() {
        let input = "use Test;\nplan 1;\nok 1, 'test';";
        let (rest, stmts) = program(input).unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 3);
    }

    #[test]
    fn parse_if_else() {
        let (rest, stmts) = program("if $x { say 1; } else { say 2; }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::If { .. }));
    }

    #[test]
    fn parse_for_loop() {
        let (rest, stmts) = program("for 1..10 -> $i { say $i; }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::For { .. }));
    }

    #[test]
    fn parse_for_loop_with_rw_param_trait() {
        let (rest, stmts) = program("for @a -> $x is rw { $x++ }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::For { .. }));
    }

    #[test]
    fn parse_compound_assign_on_method_lhs() {
        let (rest, stmts) = program("for @a -> { .key //= ++$i }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn parse_topic_method_assign_with_statement_modifier() {
        let (rest, stmts) = program(".key = 1 for @a;").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn parse_parenthesized_assign_lvalue_stmt() {
        let (rest, stmts) = program("($x = $y) = 5;").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::Block(stmts) => {
                assert_eq!(stmts.len(), 2);
                assert!(matches!(
                    &stmts[0],
                    Stmt::Expr(Expr::AssignExpr { name, expr })
                        if name == "x" && matches!(expr.as_ref(), Expr::Var(n) if n == "y")
                ));
                assert!(matches!(
                    &stmts[1],
                    Stmt::Assign { name, expr, .. }
                        if name == "x" && matches!(expr, Expr::Literal(Value::Int(5)))
                ));
            }
            other => panic!("expected block stmt, got {:?}", other),
        }
    }

    #[test]
    fn parse_parenthesized_bind_lvalue_stmt() {
        let (rest, stmts) = program("($rv1, $rv2) := |(t2);").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::Block(_)));
    }

    #[test]
    fn parse_for_loop_with_optional_pointy_param() {
        let (rest, stmts) = program("for 1..5 -> $x, $y? { say $x }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::For { .. }));
    }

    #[test]
    fn parse_for_loop_with_default_pointy_param() {
        let (rest, stmts) = program("for 1..5 -> $x, $y = 7 { say $x }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::For { .. }));
    }

    #[test]
    fn parse_for_loop_with_pointy_return_type() {
        let (rest, stmts) = program("for ^10 -> $_ --> List { take $_ }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::For { .. }));
    }

    #[test]
    fn parse_for_with_typed_unpacking_param_signature() {
        let (rest, stmts) = program("for a => 1 -> Pair $p (:$key, :$value) { $p }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::For { .. }));
    }

    #[test]
    fn parse_for_with_anonymous_unpacking_param_signature() {
        let (rest, stmts) = program("for A.new -> $ (:$x) { $x }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::For { .. }));
    }

    #[test]
    fn parse_chained_inline_modifiers_in_paren_expr() {
        let (rest, stmts) = program("my @odd = ($_ * $_ if $_ % 2 for 0..10);").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
    }

    #[test]
    fn parse_for_expression_in_parens() {
        let (rest, stmts) = program("my $x = (for ^2 { 41; 42 });").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
    }

    #[test]
    fn parse_topic_mutating_method_stmt() {
        let (rest, stmts) = program(".=fmt('%03b');").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::Expr(Expr::MethodCall { .. })));
    }

    #[test]
    fn parse_gather_for_expression() {
        let (rest, stmts) = program("my $x = (gather for ^5 { take 1 });").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
    }

    #[test]
    fn parse_labeled_for_expression() {
        let (rest, stmts) = program("my @x = (MEOW: for ^2 { 1 });").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
    }

    #[test]
    fn parse_sub_param_dollar_question() {
        let (rest, stmts) = program("sub foo($?) { 1 }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::SubDecl { .. }));
    }

    #[test]
    fn parse_topic_amp_call_inside_for_block() {
        let (rest, stmts) = program("sub foo($?) { 1 }; for 1 { .&foo() };").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 2);
        assert!(matches!(&stmts[0], Stmt::SubDecl { .. }));
        assert!(matches!(&stmts[1], Stmt::For { .. }));
    }

    #[test]
    fn parse_block_valued_colonpair() {
        let (rest, stmts) = program("my $x = :out{ .contains('ok') };").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
    }

    #[test]
    fn parse_my_regex_decl() {
        let (rest, stmts) = program("my regex rx { abc };").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::VarDecl { .. }));
    }

    #[test]
    fn parse_sub_decl() {
        let (rest, stmts) = program("sub foo($x) { return $x; }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::SubDecl { name, .. } if name == "foo"));
    }

    #[test]
    fn parse_sub_decl_with_typed_slurpy_param() {
        let (rest, stmts) = program("sub foo (Code *$block) { return $block.(); }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        if let Stmt::SubDecl { param_defs, .. } = &stmts[0] {
            assert_eq!(param_defs.len(), 1);
            assert!(param_defs[0].slurpy);
            assert_eq!(param_defs[0].type_constraint.as_deref(), Some("Code"));
        } else {
            panic!("expected SubDecl");
        }
    }

    #[test]
    fn parse_sub_decl_with_slurpy_code_param() {
        let (rest, stmts) = program("sub bar (*&block) { return &block.(); }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        if let Stmt::SubDecl { param_defs, .. } = &stmts[0] {
            assert_eq!(param_defs.len(), 1);
            assert!(param_defs[0].slurpy);
        } else {
            panic!("expected SubDecl");
        }
    }

    #[test]
    fn parse_multi_sub_with_alternate_signature_chain() {
        let src = "multi sub infix:<+> (Base $b, Exponent $e) | (Exponent $e, Base $b) { 1 }";
        let (rest, stmts) = program(src).unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::SubDecl { multi: true, .. }));
    }

    #[test]
    fn parse_class_decl() {
        let (rest, stmts) = program("class Foo { has $.x; method bar { 42 } }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::ClassDecl { name, .. } if name == "Foo"));
    }

    #[test]
    fn parse_method_decl_with_match_var_param() {
        let (rest, stmts) = program("class Foo { method TOP($/) { 1 } }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        if let Stmt::ClassDecl { body, .. } = &stmts[0] {
            assert!(matches!(&body[0], Stmt::MethodDecl { name, .. } if name == "TOP"));
        } else {
            panic!("expected ClassDecl");
        }
    }

    #[test]
    fn parse_role_decl_with_generics_and_does_clause() {
        let (rest, stmts) = program("role R2[Cool ::T] does R1[T] is ok { }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::RoleDecl { name, .. } if name == "R2"));
    }

    #[test]
    fn parse_token_decl_with_regex_like_body() {
        let (rest, stmts) = program("token TOP { <fred>+ }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::TokenDecl { name, .. } if name == "TOP"));
    }

    #[test]
    fn parse_token_decl_with_sym_variant_name() {
        let (rest, stmts) = program("token fred:sym<foo> { <sym> \\d+ }").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
        assert!(matches!(&stmts[0], Stmt::TokenDecl { name, .. } if name == "fred:sym<foo>"));
    }

    #[test]
    fn parse_make_with_token_term_in_array() {
        let (rest, stmts) = program("make [token { \"bar\" }]").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn parse_chained_constant_declarator_rhs() {
        let (rest, stmts) = program("my $a = constant $b = 42;").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 1);
    }

    #[test]
    fn parse_my_no_space() {
        let (rest, stmts) = program("my $a=0; say $a").unwrap();
        assert_eq!(rest, "");
        assert_eq!(stmts.len(), 2, "stmts: {:?}", stmts);
        if let Stmt::VarDecl { name, expr, .. } = &stmts[0] {
            assert_eq!(name, "a");
            assert!(
                matches!(expr, Expr::Literal(Value::Int(0))),
                "Expected Int(0), got {:?}",
                expr
            );
        } else {
            panic!("Expected VarDecl, got {:?}", stmts[0]);
        }
    }

    #[test]
    fn statement_memo_hits_on_reparse() {
        reset_statement_memo();
        let input = "say 42";
        let _ = statement_pub(input).unwrap();
        let (hits1, misses1, stores1) = statement_memo_stats();
        assert_eq!(hits1, 0);
        assert!(misses1 >= 1);
        assert!(stores1 >= 1);

        let _ = statement_pub(input).unwrap();
        let (hits2, _misses2, stores2) = statement_memo_stats();
        assert!(hits2 >= 1);
        assert!(stores2 >= stores1);
    }

    #[test]
    fn merge_expected_messages_deduplicates() {
        let merged = super::super::parse_result::merge_expected_messages(
            "expected foo",
            &["foo".to_string(), "bar".to_string()],
        );
        assert_eq!(merged, vec!["foo", "bar"]);
    }

    #[test]
    fn merge_expected_messages_strips_prefix_consistently() {
        let merged = super::super::parse_result::merge_expected_messages(
            "expected alpha",
            &["beta".to_string(), "gamma".to_string()],
        );
        assert_eq!(merged, vec!["alpha", "beta", "gamma"]);
    }

    #[test]
    fn statement_modifier_reports_missing_condition() {
        let base = Stmt::Expr(Expr::Literal(Value::Int(1)));
        let err = parse_statement_modifier(" if ", base).unwrap_err();
        assert!(err.message().contains("after 'if'"));
    }

    #[test]
    fn assign_stmt_reports_missing_rhs_for_compound_assign() {
        let err = assign::assign_stmt("$x +=").unwrap_err();
        assert!(err.message().contains("compound assignment"));
    }

    #[test]
    fn assign_stmt_reports_missing_method_name_for_mutating_call() {
        let err = assign::assign_stmt("$x .=").unwrap_err();
        assert!(err.message().contains("method name after '.='"));
    }

    #[test]
    fn known_call_stmt_reports_argument_parse_context() {
        simple::register_module_exports("Test");
        let err = simple::known_call_stmt("ok ,").unwrap_err();
        assert!(err.message().contains("known call arguments"));
    }

    #[test]
    fn known_call_stmt_reports_missing_comma_argument() {
        simple::register_module_exports("Test");
        let err = simple::known_call_stmt("ok(,)").unwrap_err();
        assert!(err.message().contains("known call arguments"));
    }

    #[test]
    fn known_call_stmt_reports_missing_named_argument_value() {
        simple::register_module_exports("Test");
        let err = simple::known_call_stmt("ok :foo()").unwrap_err();
        assert!(err.message().contains("named argument value"));
    }

    #[test]
    fn qualified_ident_requires_segment_after_double_colon() {
        let err = qualified_ident("Foo::").unwrap_err();
        assert!(err.message().contains("identifier after '::'"));
    }

    #[test]
    fn import_scope_is_lexical() {
        simple::reset_user_subs();
        // At top scope, "ok" is not imported
        assert!(!simple::is_imported_function("ok"));
        // Push a child scope and import Test
        simple::push_scope();
        simple::register_module_exports("Test");
        assert!(simple::is_imported_function("ok"));
        // Pop the child scope — "ok" should no longer be visible
        simple::pop_scope();
        assert!(
            !simple::is_imported_function("ok"),
            "imported function should not be visible after scope pop"
        );
    }

    #[test]
    fn import_scope_inherits_from_parent() {
        simple::reset_user_subs();
        // Import at top scope
        simple::register_module_exports("Test");
        assert!(simple::is_imported_function("ok"));
        // Push a child scope — parent imports should still be visible
        simple::push_scope();
        assert!(
            simple::is_imported_function("ok"),
            "parent scope imports should be visible in child"
        );
        simple::pop_scope();
        // Still visible in parent
        assert!(simple::is_imported_function("ok"));
        simple::reset_user_subs();
    }

    #[test]
    fn user_sub_scope_is_lexical() {
        simple::reset_user_subs();
        assert!(!simple::is_user_declared_sub("my-func"));
        // Declare a sub in a child scope
        simple::push_scope();
        simple::register_user_sub("my-func");
        assert!(simple::is_user_declared_sub("my-func"));
        // Pop — sub should no longer be visible
        simple::pop_scope();
        assert!(
            !simple::is_user_declared_sub("my-func"),
            "user sub should not be visible after scope pop"
        );
    }

    #[test]
    fn user_sub_scope_inherits_from_parent() {
        simple::reset_user_subs();
        simple::register_user_sub("outer-func");
        assert!(simple::is_user_declared_sub("outer-func"));
        // Child scope should see parent's subs
        simple::push_scope();
        assert!(
            simple::is_user_declared_sub("outer-func"),
            "parent sub should be visible in child scope"
        );
        simple::pop_scope();
        assert!(simple::is_user_declared_sub("outer-func"));
        simple::reset_user_subs();
    }
}
