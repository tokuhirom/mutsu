use super::super::parse_result::{PError, PResult, merge_expected_messages, parse_char, parse_tag};

use crate::ast::{Expr, Stmt, make_anon_sub};
use crate::value::Value;

use super::super::stmt::statement_pub;

fn is_superscript_digit(c: char) -> bool {
    matches!(
        c,
        '\u{2070}' | '\u{00B9}' | '\u{00B2}' | '\u{00B3}' | '\u{2074}'..='\u{2079}'
    )
}

use super::super::expr::{expression, or_expr_pub};
use super::super::helpers::{ws, ws1};
use super::current_line_number;
use super::misc::parse_block_body;
use super::regex::{parse_call_arg_list, scan_to_delim};

const TEST_CALLSITE_LINE_KEY: &str = "__mutsu_test_callsite_line";

fn attach_test_callsite_line(name: &str, input: &str, mut args: Vec<Expr>) -> Vec<Expr> {
    if crate::parser::stmt::simple::is_test_assertion_callable(name) {
        args.push(Expr::Binary {
            left: Box::new(Expr::Literal(Value::Str(
                TEST_CALLSITE_LINE_KEY.to_string(),
            ))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(Expr::Literal(Value::Int(current_line_number(input)))),
        });
    }
    args
}

fn make_call_expr(name: String, input: &str, args: Vec<Expr>) -> Expr {
    Expr::Call {
        name: name.clone(),
        args: attach_test_callsite_line(&name, input, args),
    }
}

fn parse_raw_braced_regex_body(input: &str) -> PResult<'_, String> {
    let after_open = input
        .strip_prefix('{')
        .ok_or_else(|| PError::expected("regex body"))?;
    if let Some((body, rest)) = scan_to_delim(after_open, '{', '}', true) {
        return Ok((rest, body.trim().to_string()));
    }
    Err(PError::expected("regex closing delimiter"))
}

/// Parse `::Foo` class literal (type object reference) or `::($expr)` indirect type lookup.
pub(super) fn class_literal(input: &str) -> PResult<'_, Expr> {
    if !input.starts_with("::") {
        return Err(PError::expected("class literal"));
    }
    let rest = &input[2..];
    // Handle ::?PACKAGE and ::?MODULE — compile-time pseudo-packages
    if let Some(after) = rest.strip_prefix("?PACKAGE")
        && after
            .chars()
            .next()
            .is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-')
    {
        return Ok((after, Expr::Var("?PACKAGE".to_string())));
    }
    if let Some(after) = rest.strip_prefix("?MODULE")
        && after
            .chars()
            .next()
            .is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '-')
    {
        return Ok((after, Expr::Var("?MODULE".to_string())));
    }
    // Handle ::($expr) — indirect name lookup
    if let Some(after_paren) = rest.strip_prefix('(') {
        let (r, _) = ws(after_paren)?;
        let (r, inner) = expression(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = parse_char(r, ')')?;
        return Ok((r, Expr::IndirectTypeLookup(Box::new(inner))));
    }
    let (rest, name) = super::super::stmt::ident_pub(rest)?;
    // Handle qualified names: ::Foo::Bar
    let mut full_name = name;
    let mut r = rest;
    while r.starts_with("::") {
        let r2 = &r[2..];
        if let Ok((r2, part)) = super::super::stmt::ident_pub(r2) {
            full_name = format!("{}::{}", full_name, part);
            r = r2;
        } else {
            break;
        }
    }
    // Type smileys: ::Foo:U, ::Foo:D, ::Foo:_
    if (r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_"))
        && !r[2..].starts_with('<')
    {
        let smiley = &r[..2];
        full_name = format!("{}{}", full_name, smiley);
        r = &r[2..];
    }
    Ok((r, Expr::BareWord(full_name)))
}

pub(super) fn whatever(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '*')?;
    // ** is HyperWhatever (but not **N which is power op)
    if let Some(after) = input.strip_prefix('*') {
        // Make sure it's not *** or **N (power op with operand)
        if after.starts_with('*') {
            return Err(PError::expected("whatever"));
        }
        // **<digit> or **$ would be power op, but **) or **, or end is HyperWhatever
        if !after.is_empty()
            && !after.starts_with(')')
            && !after.starts_with(',')
            && !after.starts_with(';')
            && !after.starts_with('}')
            && !after.starts_with(' ')
            && !after.starts_with('\n')
            && !after.starts_with('\t')
            && !after.starts_with('\r')
        {
            return Err(PError::expected("whatever (not **)"));
        }
        return Ok((after, Expr::HyperWhatever));
    }
    Ok((input, Expr::Whatever))
}

/// Parse keywords that are values: True, False, Nil, Any, Inf, NaN, etc.
pub(super) fn keyword_literal(input: &str) -> PResult<'_, Expr> {
    // Try each keyword, ensuring it's not followed by alphanumeric (word boundary)
    // Also reject if followed by `(` to prevent treating e() as a constant
    let try_kw = |kw: &str, val: Value| -> PResult<'_, Expr> {
        let (rest, _) = parse_tag(input, kw)?;
        // Check word boundary (superscript digits are NOT word chars)
        if let Some(c) = rest.chars().next()
            && (c.is_alphanumeric() || c == '_' || c == '-')
            && !is_superscript_digit(c)
        {
            return Err(PError::expected("word boundary"));
        }
        // Reject if followed by `(` - that's a function call, not a constant
        if rest.starts_with('(') {
            return Err(PError::expected("not a function call"));
        }
        // Reject if followed by `=>` (pair key context)
        let trimmed = rest.trim_start();
        if trimmed.starts_with("=>") && !trimmed.starts_with("==>") {
            return Err(PError::expected("not a pair key"));
        }
        Ok((rest, Expr::Literal(val)))
    };

    if let Ok(r) = try_kw("True", Value::Bool(true)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("False", Value::Bool(false)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("Nil", Value::Nil) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("Empty", Value::Slip(std::sync::Arc::new(vec![]))) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("Any", Value::Package("Any".to_string())) {
        return Ok(r);
    }
    // Unicode: ∅ (U+2205 EMPTY SET)
    if input.starts_with('\u{2205}') {
        return Ok((
            &input['\u{2205}'.len_utf8()..],
            Expr::Literal(Value::set(std::collections::HashSet::new())),
        ));
    }
    if let Ok(r) = try_kw("Inf", Value::Num(f64::INFINITY)) {
        return Ok(r);
    }
    // ∞ (U+221E INFINITY)
    if input.starts_with('\u{221E}') {
        return Ok((
            &input['\u{221E}'.len_utf8()..],
            Expr::Literal(Value::Num(f64::INFINITY)),
        ));
    }
    if let Ok(r) = try_kw("-Inf", Value::Num(f64::NEG_INFINITY)) {
        return Ok(r);
    }
    // Note: -∞ is handled by prefix negation + ∞ literal, not as a special case,
    // so that -∞² parses correctly as -(∞²) not (-∞)².
    if let Ok(r) = try_kw("NaN", Value::Num(f64::NAN)) {
        return Ok(r);
    }
    // rand — generates a random number (term)
    if input.starts_with("rand")
        && !input[4..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
    {
        return Ok((
            &input[4..],
            Expr::Call {
                name: "rand".to_string(),
                args: vec![],
            },
        ));
    }
    // now — returns current time as Instant (term)
    if input.starts_with("now")
        && !input[3..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
    {
        return Ok((
            &input[3..],
            Expr::Call {
                name: "now".to_string(),
                args: vec![],
            },
        ));
    }
    // time — returns current epoch time as Int (term)
    if input.starts_with("time")
        && !input[4..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
    {
        return Ok((
            &input[4..],
            Expr::Call {
                name: "time".to_string(),
                args: vec![],
            },
        ));
    }
    // INIT expr — statement prefix phaser, evaluates expr at init time
    // In an interpreter, INIT time ≈ run time, so just evaluate the expression.
    if input.starts_with("INIT")
        && !input[4..].starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '-')
    {
        let r = &input[4..];
        let (r, _) = super::super::helpers::ws(r)?;
        if !r.starts_with('{') {
            let (r, expr) = super::super::expr::expression_no_sequence(r)?;
            return Ok((r, expr));
        }
    }
    if let Ok(r) = try_kw("pi", Value::Num(std::f64::consts::PI)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("π", Value::Num(std::f64::consts::PI)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("tau", Value::Num(std::f64::consts::TAU)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("τ", Value::Num(std::f64::consts::TAU)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("e", Value::Num(std::f64::consts::E)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("i", Value::Complex(0.0, 1.0)) {
        return Ok(r);
    }
    if let Ok(r) = try_kw("\u{1D452}", Value::Num(std::f64::consts::E)) {
        return Ok(r);
    }
    Err(PError::expected("keyword literal"))
}

/// Check if a name is a Raku keyword (not a function call).
pub(super) fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "if" | "unless"
            | "for"
            | "while"
            | "until"
            | "given"
            | "when"
            | "loop"
            | "repeat"
            | "try"
            | "do"
            | "gather"
            | "sub"
            | "my"
            | "our"
            | "has"
            | "class"
            | "role"
            | "module"
            | "use"
            | "need"
            | "import"
            | "require"
            | "return"
            | "last"
            | "next"
            | "redo"
            | "die"
            | "say"
            | "print"
            | "put"
            | "note"
            | "with"
            | "without"
            | "supply"
            | "react"
            | "whenever"
            | "start"
            | "quietly"
            | "sink"
            | "let"
    )
}

/// Check if a name is a listop (can take args without parens).
pub(super) fn is_listop(name: &str) -> bool {
    matches!(
        name,
        "shift"
            | "unshift"
            | "push"
            | "pop"
            | "grep"
            | "map"
            | "sort"
            | "first"
            | "any"
            | "all"
            | "none"
            | "one"
            | "print"
            | "say"
            | "put"
            | "note"
            | "return"
            | "die"
            | "fail"
            | "warn"
            | "make"
            | "take"
            | "emit"
            | "split"
            | "index"
            | "join"
            | "reverse"
            | "min"
            | "max"
            | "sum"
            | "pick"
            | "roll"
            | "sleep"
            | "dir"
    ) || is_expr_listop(name)
}

/// Functions that take multiple comma-separated expression arguments in listop style.
/// These are parsed with full expression arguments (not just primaries).
/// Test/Test::Util functions are NOT listed here — they are registered dynamically
/// via `register_module_exports()` when `use Test` / `use Test::Util` is parsed.
pub(super) fn is_expr_listop(name: &str) -> bool {
    matches!(
        name,
        "EVAL" | "flat" | "slip" | "run" | "shell" | "cross" | "await" | "dir"
    ) || crate::parser::stmt::simple::is_imported_function(name)
}

/// Check if a name is an infix word operator (should not be treated as a listop call).
fn is_infix_word_op(name: &str) -> bool {
    matches!(
        name,
        "Z" | "X"
            | "R"
            | "x"
            | "xx"
            | "eq"
            | "ne"
            | "lt"
            | "gt"
            | "le"
            | "ge"
            | "cmp"
            | "leg"
            | "and"
            | "or"
            | "not"
            | "div"
            | "mod"
            | "gcd"
            | "lcm"
            | "but"
            | "does"
            | "min"
            | "max"
            | "ff"
            | "fff"
            | "before"
            | "after"
            | "andthen"
            | "orelse"
            | "notandthen"
    )
}

/// Check if input starts with a statement modifier keyword.
fn is_stmt_modifier_ahead(input: &str) -> bool {
    let input = input.trim_start();
    for kw in &["if", "unless", "for", "while", "until", "given", "when"] {
        if input.starts_with(kw)
            && !input
                .as_bytes()
                .get(kw.len())
                .is_some_and(|&c| c.is_ascii_alphanumeric() || c == b'_' || c == b'-')
        {
            return true;
        }
    }
    false
}

/// Parse expression listop arguments: comma-separated full expressions.
/// Stops at statement modifiers, semicolons, and closing brackets.
fn parse_expr_listop_args(input: &str, name: String) -> PResult<'_, Expr> {
    // Raku listop `slip ...` takes a single expression argument, which may
    // itself be a comma expression (e.g. `slip (2,3), 4`).
    if name == "slip" {
        let (r, first) = expression(input).map_err(|err| PError {
            messages: merge_expected_messages("expected listop argument expression", &err.messages),
            remaining_len: err.remaining_len.or(Some(input.len())),
        })?;
        let mut exprs = vec![first];
        let mut r = r;
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with(',') || r2.starts_with(",,") {
                break;
            }
            let r2 = &r2[1..];
            let (r2, _) = ws(r2)?;
            if r2.is_empty()
                || r2.starts_with(';')
                || r2.starts_with('}')
                || r2.starts_with(')')
                || is_stmt_modifier_ahead(r2)
            {
                break;
            }
            let (r2, expr) = expression(r2).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected listop argument expression after ','",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r2.len())),
            })?;
            exprs.push(expr);
            r = r2;
        }
        let arg = if exprs.len() == 1 {
            exprs.remove(0)
        } else {
            Expr::ArrayLiteral(exprs)
        };
        return Ok((r, make_call_expr(name, input, vec![arg])));
    }

    let (r, first) = expression(input).map_err(|err| PError {
        messages: merge_expected_messages("expected listop argument expression", &err.messages),
        remaining_len: err.remaining_len.or(Some(input.len())),
    })?;
    let mut args = vec![first];
    let mut r = r;
    loop {
        let (r2, _) = ws(r)?;
        if !r2.starts_with(',') || r2.starts_with(",,") {
            break;
        }
        let r2 = &r2[1..];
        let (r2, _) = ws(r2)?;
        // Stop at terminators
        if r2.is_empty()
            || r2.starts_with(';')
            || r2.starts_with('}')
            || r2.starts_with(')')
            || is_stmt_modifier_ahead(r2)
        {
            break;
        }
        let (r2, arg) = expression(r2).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected listop argument expression after ','",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r2.len())),
        })?;
        args.push(arg);
        r = r2;
    }
    Ok((r, make_call_expr(name, input, args)))
}

fn parse_listop_arg(input: &str) -> PResult<'_, Expr> {
    // Try to parse a primary expression (variable, literal, call, etc.)
    // but stop if we hit a statement modifier
    if is_stmt_modifier_ahead(input) {
        return Err(PError::expected("listop argument"));
    }

    // In argument context, .5 should parse as 0.5 (not topic method call)
    if input.starts_with('.') && input.len() > 1 && input.as_bytes()[1].is_ascii_digit() {
        let dot_rest = &input[1..]; // skip '.'
        let end = dot_rest
            .find(|c: char| !c.is_ascii_digit() && c != '_')
            .unwrap_or(dot_rest.len());
        let frac_str = &dot_rest[..end];
        let rest = &dot_rest[end..];
        // Parse as Rat: 0.DIGITS
        let frac_clean: String = frac_str.chars().filter(|c| *c != '_').collect();
        let denom = 10_i64.pow(frac_clean.len() as u32);
        let numer: i64 = frac_clean.parse().unwrap_or(0);
        return Ok((rest, Expr::Literal(crate::value::Value::Rat(numer, denom))));
    }

    // Parse a single term with prefix/postfix operators, but no infix operators.
    // This keeps listop precedence behavior (e.g. `shift @a + 1` parses as
    // `(shift @a) + 1`) while still allowing argument postfix chains like
    // `chmod $file.IO.mode, $other`.
    super::super::expr::term_expr(input)
}

pub(super) fn identifier_or_call(input: &str) -> PResult<'_, Expr> {
    let (rest, name) = super::super::stmt::parse_raku_ident(input)?;
    let name = name.to_string();

    // Handle special expression keywords before qualified name resolution
    match name.as_str() {
        "infix" | "prefix" | "postfix" => {
            // infix:<OP>(args) — operator reference
            if rest.starts_with(":<") || rest.starts_with(":<<") {
                let r = &rest[1..]; // skip ':'
                let (delim_start, delim_end) = if r.starts_with("<<") {
                    ("<<", ">>")
                } else {
                    ("<", ">")
                };
                let r = &r[delim_start.len()..];
                if let Some(end_pos) = r.find(delim_end) {
                    let op_name = &r[..end_pos];
                    let r = &r[end_pos + delim_end.len()..];
                    let full_name = format!("{}:<{}>", name, op_name);
                    // Check if followed by (args)
                    let (r, _) = ws(r)?;
                    if let Some(r) = r.strip_prefix('(') {
                        let (r, _) = ws(r)?;
                        if let Some(r) = r.strip_prefix(')') {
                            return Ok((
                                r,
                                Expr::Call {
                                    name: full_name,
                                    args: vec![],
                                },
                            ));
                        }
                        let (r, first) = expression(r)?;
                        let mut args = vec![first];
                        let mut r = r;
                        loop {
                            let (r2, _) = ws(r)?;
                            if let Some(r2) = r2.strip_prefix(')') {
                                r = r2;
                                break;
                            }
                            if let Some(r2) = r2.strip_prefix(',') {
                                let (r2, _) = ws(r2)?;
                                if let Some(r2) = r2.strip_prefix(')') {
                                    r = r2;
                                    break;
                                }
                                let (r2, arg) = expression(r2)?;
                                args.push(arg);
                                r = r2;
                            } else {
                                r = r2;
                                break;
                            }
                        }
                        return Ok((
                            r,
                            Expr::Call {
                                name: full_name,
                                args,
                            },
                        ));
                    }
                    return Ok((r, Expr::BareWord(full_name)));
                }
            }
        }
        "try" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::Try { body, catch: None }));
            }
            // try EXPR — wrap expression in try
            let (r, expr) = expression(r)?;
            return Ok((
                r,
                Expr::Try {
                    body: vec![Stmt::Expr(expr)],
                    catch: None,
                },
            ));
        }
        "do" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::DoBlock { body, label: None }));
            }
            // do if/unless/given/for/while — wrap the control flow statement
            {
                let is_ctrl = |s: &str| {
                    for kw in &["if", "unless", "given", "for", "while", "until"] {
                        if s.starts_with(kw)
                            && !s.as_bytes().get(kw.len()).is_some_and(|&c| {
                                c.is_ascii_alphanumeric() || c == b'_' || c == b'-'
                            })
                        {
                            return true;
                        }
                    }
                    false
                };
                if is_ctrl(r)
                    && let Ok((r, stmt)) = super::super::stmt::statement_pub(r)
                {
                    return Ok((r, Expr::DoStmt(Box::new(stmt))));
                }
            }
            // do STMT — wrap an assignment or other statement
            if let Ok((r, stmt)) = super::super::stmt::statement_pub(r) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
            // do EXPR — just evaluate the expression
            let (r, expr) = expression(r)?;
            return Ok((r, expr));
        }
        "if" => {
            // One-pass parsing rule: `if` is a control keyword only when followed by whitespace.
            // This allows user-defined `sub if` to be called as `if()` / `if;`.
            if let Ok((r, _)) = ws1(rest) {
                let (r, cond) = super::super::expr::expression(r)?;
                let (r, _) = ws(r)?;
                let (r, then_branch) = parse_block_body(r)?;
                let (r, _) = ws(r)?;
                // Check for else
                let (r, else_branch) = if let Some(r2) = super::super::stmt::keyword("else", r) {
                    let (r2, _) = ws(r2)?;
                    let (r2, body) = parse_block_body(r2)?;
                    (r2, body)
                } else {
                    (r, Vec::new())
                };
                return Ok((
                    r,
                    Expr::DoStmt(Box::new(crate::ast::Stmt::If {
                        cond,
                        then_branch,
                        else_branch,
                    })),
                ));
            }
        }
        "unless" => {
            if let Ok((r, _)) = ws1(rest) {
                let (r, cond) = super::super::expr::expression(r)?;
                let (r, _) = ws(r)?;
                let (r, body) = parse_block_body(r)?;
                return Ok((
                    r,
                    Expr::DoStmt(Box::new(crate::ast::Stmt::If {
                        cond: Expr::Unary {
                            op: crate::token_kind::TokenKind::Bang,
                            expr: Box::new(cond),
                        },
                        then_branch: body,
                        else_branch: Vec::new(),
                    })),
                ));
            }
        }
        "for" => {
            if let Ok((r, stmt)) = super::super::stmt::for_stmt_pub(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "my" | "our" | "state" => {
            // my/our/state declaration in expression context
            // e.g., (my $x = 5) or (state $x = 3)
            if let Ok((r, stmt)) = super::super::stmt::my_decl_expr_pub(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "constant" => {
            // constant declaration in expression context
            if let Ok((r, stmt)) = super::super::stmt::constant_decl_pub(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "enum" => {
            // enum in expression context: enum < ... > or enum :: < ... >
            if let Ok((r, stmt)) = super::super::stmt::decl::enum_decl(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "anon" => {
            // anon enum < ... > in expression context
            if let Ok((r, stmt)) = super::super::stmt::decl::anon_enum_decl(input) {
                return Ok((r, Expr::DoStmt(Box::new(stmt))));
            }
        }
        "sub" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, make_anon_sub(body)));
            }
            // sub with params: sub ($x, $y) { ... }
            if r.starts_with('(') {
                let (r2, params_body) = parse_anon_sub_with_params(r).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected anonymous sub parameter list/body",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r.len())),
                })?;
                return Ok((r2, params_body));
            }
            // sub not followed by { or ( in expression context — not valid
            return Err(PError::expected_at("anonymous sub body '{' or '('", r));
        }
        "token" | "regex" | "rule" => {
            // token/rule term literal: token { ... } / rule { ... }
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, pat) = parse_raw_braced_regex_body(r)?;
                return Ok((r, Expr::Literal(Value::Regex(pat))));
            }
        }
        "gather" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((r, Expr::Gather(body)));
            }
            // gather <statement> (e.g. `gather for ^5 { ... }`)
            if let Ok((r, stmt)) = super::super::stmt::statement_pub(r) {
                return Ok((r, Expr::Gather(vec![stmt])));
            }
        }
        "die" | "fail" => {
            let (r, _) = ws(rest)?;
            // die/fail with no argument
            if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
                return Ok((r, Expr::Call { name, args: vec![] }));
            }
            let (r, arg) = expression(r)?;
            return Ok((
                r,
                Expr::Call {
                    name,
                    args: vec![arg],
                },
            ));
        }
        "quietly" => {
            let (r, _) = ws(rest)?;
            // quietly expr — defer evaluation so quietly can control warning behavior
            let (r, expr) = expression(r)?;
            let wrapped = match expr {
                Expr::AnonSub(_) | Expr::AnonSubParams { .. } => expr,
                other => make_anon_sub(vec![Stmt::Expr(other)]),
            };
            return Ok((
                r,
                Expr::Call {
                    name: name.clone(),
                    args: vec![wrapped],
                },
            ));
        }
        "sink" => {
            let (r, _) = ws(rest)?;
            // sink expr — evaluate expression and discard result
            let (r, expr) = expression(r)?;
            return Ok((
                r,
                Expr::Call {
                    name: name.clone(),
                    args: vec![expr],
                },
            ));
        }
        "start" => {
            let (r, _) = ws(rest)?;
            if r.starts_with('{') {
                let (r, body) = parse_block_body(r)?;
                return Ok((
                    r,
                    Expr::Call {
                        name: "start".to_string(),
                        args: vec![make_anon_sub(body)],
                    },
                ));
            }
            // start <statement> — wrap the next statement in a block
            if let Ok((r, stmt)) = statement_pub(r) {
                return Ok((
                    r,
                    Expr::Call {
                        name: "start".to_string(),
                        args: vec![make_anon_sub(vec![stmt])],
                    },
                ));
            }
        }
        "last" => {
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Last,
                    label: None,
                },
            ));
        }
        "next" => {
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Next,
                    label: None,
                },
            ));
        }
        "redo" => {
            return Ok((
                rest,
                Expr::ControlFlow {
                    kind: crate::ast::ControlFlowKind::Redo,
                    label: None,
                },
            ));
        }
        _ => {}
    }

    // Labeled loop in expression context, e.g. `MEOW: for ^10 { ... }`
    if name.chars().all(|c| c.is_ascii_uppercase() || c == '_') {
        let (r_ws, _) = ws(rest)?;
        if r_ws.starts_with(':')
            && let Ok((r, stmt)) = super::super::stmt::labeled_loop_stmt_pub(input)
        {
            return Ok((r, Expr::DoStmt(Box::new(stmt))));
        }
    }

    // Check for :: qualified name (e.g. Foo::Bar, CORE::<&run>)
    let (rest, name) = {
        let mut full_name = name;
        let mut r = rest;
        while r.starts_with("::") {
            let after = &r[2..];
            // Handle ::<SYMBOL> subscript syntax (e.g., CORE::<&run>)
            if let Some(after_bracket) = after.strip_prefix('<')
                && let Some(end) = after_bracket.find('>')
            {
                let symbol = &after_bracket[..end];
                full_name.push_str("::");
                full_name.push_str(symbol);
                r = &after_bracket[end + 1..];
                continue;
            }
            if let Ok((rest2, part)) = super::super::stmt::parse_raku_ident(after) {
                full_name.push_str("::");
                full_name.push_str(part);
                r = rest2;
            } else if after.starts_with('.')
                || after.is_empty()
                || after.starts_with(';')
                || after.starts_with(')')
                || after.starts_with(',')
                || after.starts_with(' ')
            {
                // Trailing `::` stash lookup form (e.g. `A::`, `MY::`).
                // Pseudo packages remain supported, and ordinary package stashes are accepted
                // for parse compatibility.
                let mut stash_name = full_name.clone();
                stash_name.push_str("::");
                return Ok((after, Expr::PseudoStash(stash_name)));
            } else {
                return Err(PError::expected_at("identifier after '::'", after));
            }
        }
        // Type smileys: TypeName:U, TypeName:D, TypeName:_
        if (r.starts_with(":D") || r.starts_with(":U") || r.starts_with(":_"))
            && !r[2..].starts_with('<')
            && full_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
        {
            let smiley = &r[..2];
            full_name.push_str(smiley);
            r = &r[2..];
        }
        (r, full_name)
    };

    // Check if followed by `(` for function call
    if rest.starts_with('(') {
        let (rest, _) = parse_char(rest, '(')?;
        let (rest, _) = ws(rest)?;
        let (rest, args) = parse_call_arg_list(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, make_call_expr(name, input, args)));
    }

    // Bareword followed by => — Pair constructor (higher precedence than operators)
    // e.g., b => "foo" should parse as Pair even in `%a ~~ b => "foo"`
    let (r, _) = ws(rest)?;
    if r.starts_with("=>") && !r.starts_with("==>") {
        let r2 = &r[2..];
        let (r2, _) = ws(r2)?;
        let (r2, value) = or_expr_pub(r2)?;
        return Ok((
            r2,
            Expr::Binary {
                left: Box::new(Expr::Literal(Value::Str(name))),
                op: crate::token_kind::TokenKind::FatArrow,
                right: Box::new(value),
            },
        ));
    }

    // Bareword followed by block { ... } and comma/colon — function call with block arg
    // e.g., map { $_ * 2 }, @arr  or  map { $_ * 2 }: @arr
    if r.starts_with('{')
        && !is_keyword(&name)
        && let Ok((r2, block_body)) = parse_block_body(r)
    {
        if name == "BEGIN" {
            return Ok((
                r2,
                make_call_expr(name, input, vec![make_anon_sub(block_body)]),
            ));
        }
        let (r3, _) = ws(r2)?;
        if let Some(r3) = r3.strip_prefix(',').or_else(|| r3.strip_prefix(':')) {
            // Consume comma and remaining args
            let (r3, _) = ws(r3)?;
            let mut args = vec![make_anon_sub(block_body)];
            let (mut r3, first_arg) = expression(r3)?;
            args.push(first_arg);
            loop {
                let (r4, _) = ws(r3)?;
                if !r4.starts_with(',') {
                    return Ok((r3, make_call_expr(name, input, args)));
                }
                let r4 = &r4[1..];
                let (r4, _) = ws(r4)?;
                if r4.starts_with(';') || r4.is_empty() || r4.starts_with('}') {
                    return Ok((r4, make_call_expr(name, input, args)));
                }
                let (r4, next_arg) = expression(r4)?;
                args.push(next_arg);
                r3 = r4;
            }
        }
        // Block without trailing comma — return as separate expressions
        // Fall through to BareWord
    }

    // Check for listop: bareword followed by space and argument (but not statement modifier)
    // e.g., shift @a, push @a, 42, etc.
    // Skip when followed by '.' — `func.method` is `(func()).method`, not `func(.method)`.
    if is_listop(&name)
        && !r.is_empty()
        && !r.starts_with(';')
        && !r.starts_with('}')
        && !r.starts_with(')')
        && !r.starts_with(',')
        && !r.starts_with('.')
    {
        // Check if next token is a statement modifier keyword
        if !is_stmt_modifier_ahead(r) {
            // Expression listops (ok, is, diag, etc.) parse full expressions as args
            if is_expr_listop(&name) {
                return parse_expr_listop_args(r, name);
            }
            // Try to parse an argument
            let (r2, arg) = parse_listop_arg(r).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected listop argument expression",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
            })?;
            let mut args = vec![arg];
            let mut rest_after = r2;
            loop {
                let (r3, _) = ws(rest_after)?;
                if !r3.starts_with(',') {
                    break;
                }
                let r3 = &r3[1..];
                let (r3, _) = ws(r3)?;
                if r3.starts_with(';')
                    || r3.starts_with('}')
                    || r3.starts_with(')')
                    || r3.is_empty()
                {
                    break;
                }
                let (r3, rest_arg) = parse_listop_arg(r3)?;
                args.push(rest_arg);
                rest_after = r3;
            }
            return Ok((rest_after, make_call_expr(name, input, args)));
        }
    }

    // User-defined function call in listop style: bareword followed by space and a term
    // e.g., `äöü 17` or `my-func $x`
    // Exclude single uppercase letters (like Z, X, R) which are infix meta-operators.
    if !is_keyword(&name)
        && !is_infix_word_op(&name)
        && !r.is_empty()
        && !r.starts_with(';')
        && !r.starts_with('}')
        && !r.starts_with(')')
        && !r.starts_with(',')
        && !r.starts_with('.')
        && !is_stmt_modifier_ahead(r)
    {
        let is_user_sub = crate::parser::stmt::simple::is_user_declared_sub(&name);
        let next = r.chars().next().unwrap();
        let hyphen_forward_call = !is_user_sub && name.contains('-');
        if is_user_sub && let Ok((r2, expr)) = parse_expr_listop_args(r, name.clone()) {
            return Ok((r2, expr));
        }
        if hyphen_forward_call && let Ok((r2, expr)) = parse_expr_listop_args(r, name.clone()) {
            return Ok((r2, expr));
        }
        // Only trigger if next token starts a term (not an operator)
        if (next == '$'
            || next == '@'
            || next == '%'
            || next == '&'
            || next == ':'
            || next == '\''
            || next == '"'
            || next == '‘'
            || next == '’'
            || next == '“'
            || next == '”'
            || next == '｢'
            || next == '('
            || next.is_ascii_digit()
            || hyphen_forward_call
            || is_user_sub)
            && let Ok((r2, arg)) = parse_listop_arg(r)
        {
            // For user subs, collect comma-separated args
            if is_user_sub {
                return parse_expr_listop_args(r, name);
            }
            let mut args = vec![arg];
            let mut rest_after = r2;
            loop {
                let (r3, _) = ws(rest_after)?;
                if !r3.starts_with(',') {
                    break;
                }
                let r3 = &r3[1..];
                let (r3, _) = ws(r3)?;
                if r3.starts_with(';')
                    || r3.starts_with('}')
                    || r3.starts_with(')')
                    || r3.is_empty()
                {
                    break;
                }
                let (r3, next_arg) = parse_listop_arg(r3)?;
                args.push(next_arg);
                rest_after = r3;
            }
            return Ok((rest_after, make_call_expr(name, input, args)));
        }
    }

    let is_terminator = rest.starts_with(';')
        || rest.starts_with('}')
        || rest.starts_with(')')
        || is_stmt_modifier_ahead(rest)
        || rest.trim_start().is_empty();

    // User-declared and imported subs can be called with no args as bare words
    // in statement position (e.g., `make-temp-dir;`).
    if (crate::parser::stmt::simple::is_user_declared_sub(&name)
        || crate::parser::stmt::simple::is_imported_function(&name))
        && is_terminator
    {
        return Ok((rest, make_call_expr(name, input, vec![])));
    }

    // Functions that can be called with no arguments as bare words
    if matches!(name.as_str(), "await") && is_terminator {
        return Ok((rest, make_call_expr(name, input, vec![])));
    }

    // Method-like: .new, .elems etc. is handled at expression level
    Ok((rest, Expr::BareWord(name)))
}

/// Parse anonymous sub with params: sub ($x, $y) { ... }
pub(super) fn parse_anon_sub_with_params(input: &str) -> PResult<'_, Expr> {
    let (r, _) = parse_char(input, '(')?;
    let (r, _) = ws(r)?;
    let (r, param_defs) = super::super::stmt::parse_param_list_pub(r)?;
    let params: Vec<String> = param_defs.iter().map(|p| p.name.clone()).collect();
    parse_anon_sub_rest(r, params, param_defs)
}

fn parse_anon_sub_rest(
    input: &str,
    params: Vec<String>,
    param_defs: Vec<crate::ast::ParamDef>,
) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    let (r, _) = parse_char(r, ')')?;
    let (r, _) = ws(r)?;
    let (r, body) = parse_block_body(r)?;
    if params.is_empty() {
        Ok((r, make_anon_sub(body)))
    } else {
        Ok((
            r,
            Expr::AnonSubParams {
                params,
                param_defs,
                body,
            },
        ))
    }
}
