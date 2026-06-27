//! Statement-list and block parsing.
//!
//! Moved out of `stmt/mod.rs` (§7-8 cohesive split). These functions parse
//! blocks (`{ ... }`), statement lists (with mainline/unit capture handling),
//! and best-effort partial parsing for module loading. Re-exported from
//! `mod.rs` at their original visibility.

use super::*;

/// Parse a block: { stmts }
/// Pushes/pops a lexical import scope so that `use` inside a block
/// only affects that block and its children.
pub(crate) fn block(input: &str) -> PResult<'_, Vec<Stmt>> {
    let (input, _) = parse_char(input, '{')?;
    simple::push_scope();
    let result = block_inner(input);
    simple::pop_scope();
    result
}

pub(crate) fn block_inner(input: &str) -> PResult<'_, Vec<Stmt>> {
    let (input, stmts) = stmt_list_with_mode(input, false, true)?;
    let (input, _) = ws(input)?;
    let (input, _) = parse_char(input, '}')?;
    Ok((input, stmts))
}

/// Public accessor for stmt_list (used by primary.rs for block expressions).
pub(crate) fn stmt_list_pub(input: &str) -> PResult<'_, Vec<Stmt>> {
    stmt_list_with_mode(input, false, false)
}

/// Parse statements in best-effort mode: return all successfully parsed
/// statements even if a parse error is encountered partway through.
/// When a statement fails to parse, skip forward to the next statement
/// boundary and continue parsing. Used by `load_module` so that
/// partially-parseable `.rakumod` files still export their functions.
pub(crate) fn stmt_list_partial(input: &str) -> (Vec<Stmt>, Option<String>) {
    let mut stmts = Vec::new();
    let mut rest = input;
    while let Ok((r, _)) = ws_bol(rest) {
        let r = consume_semicolons(r);
        let Ok((r, _)) = ws_bol(r) else { break };
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

/// Check if a statement ends with a block (closing `}`).
/// These "statement-ending blocks" can omit the semicolon only if followed
/// by a newline, semicolon, closing brace, comma, or end-of-input.
fn is_block_ending_stmt(stmt: &Stmt) -> bool {
    match stmt {
        // Sub declarations only count if they have a body (not forward declarations).
        // sub_decl does NOT call parse_statement_modifier, so trailing separators
        // remain in the rest for us to check.
        Stmt::SubDecl { body, .. } => !body.is_empty(),
        // `when`/`default` block statements take a `{...}` body and, unlike
        // `if`/`for`/etc., are not followed by any same-line continuation
        // keyword (no `else`/`elsif`) and do not consume trailing statement
        // modifiers. So `when 1 { } when 2 { }` on one line (no `;`) is a
        // "Strange text after block" error in Raku, matching `given $x { ... }`.
        Stmt::When { .. } | Stmt::Default(_) => true,
        // Note: MethodDecl is excluded because it can arrive via my_decl which calls
        // parse_statement_modifier, consuming the trailing separator before we see it.
        // Stmt::Block, Stmt::If, Stmt::For, ClassDecl, RoleDecl, Package, etc. are
        // also excluded for similar reasons -- they may go through paths that consume
        // trailing separators before returning to stmt_list_with_mode.
        _ => false,
    }
}

/// Check that the remaining input has a proper statement separator
/// (newline, semicolon, comma, closing brace, or end-of-input)
/// before any non-whitespace code on the same line.
fn has_statement_separator(rest: &str) -> bool {
    for ch in rest.chars() {
        match ch {
            // Horizontal whitespace -- keep scanning
            ' ' | '\t' => continue,
            // Valid separators: newline, semicolon, closing brace, comma,
            // or `#` (start of line comment, effectively end-of-statement)
            '\n' | '\r' | ';' | '}' | ',' | '#' => return true,
            // Any other non-whitespace on the same line -> missing separator
            _ => return false,
        }
    }
    // End of input is a valid separator
    true
}

pub(crate) fn stmt_list_with_mode(
    input: &str,
    allow_mainline_capture: bool,
    emit_setline: bool,
) -> PResult<'_, Vec<Stmt>> {
    let mut stmts = Vec::new();
    let mut rest = input;
    let mut saw_compunit_declarator = false;
    loop {
        // Use ws_bol (beginning-of-line) so that Pod blocks at the start of
        // the file or after newlines are consumed.  The regular ws() defaults
        // to at_line_start=false, which is correct for mid-line positions.
        let (r, _) = ws_bol(rest)?;
        // Consume any standalone semicolons
        let r = consume_semicolons(r);
        let (r, _) = ws_bol(r)?;
        // End of block or input
        if r.is_empty() || r.starts_with('}') {
            return Ok((r, stmts));
        }
        if allow_mainline_capture
            && let Ok((after_decl, mut main_sub)) = sub::top_level_main_semicolon_decl(r)
        {
            if saw_compunit_declarator {
                return Err(PError::raw(
                    "X::UnitScope::TooLate: A unit-scoped sub declaration is too late in the compilation unit"
                        .to_string(),
                    Some(r.len()),
                ));
            }
            let (tail_rest, tail_stmts) = stmt_list_with_mode(after_decl, false, emit_setline)?;
            if let Stmt::SubDecl { body, .. } = &mut main_sub {
                *body = tail_stmts;
            }
            stmts.push(main_sub);
            return Ok((tail_rest, stmts));
        }
        if allow_mainline_capture
            && let Some(after_multi) = keyword("multi", r)
            && let Ok((after_multi, _)) = ws1(after_multi)
            && sub::top_level_main_semicolon_decl(after_multi).is_ok()
        {
            return Err(PError::raw(
                "X::UnitScope::Invalid: A unit-scoped sub definition is not allowed except on a MAIN sub; \
                 Please use the block form. If you did not mean to declare a unit-scoped sub, \
                 perhaps you accidentally placed a semicolon after routine's definition?"
                    .to_string(),
                Some(r.len()),
            ));
        }
        // `unit class`/`unit role`/`unit grammar` (semicolon form): the rest of
        // the compilation unit is the body of the declared type — capture it
        // like `unit sub MAIN`. Without this, a top-level `has`/`method` after
        // `unit class Foo;` is seen as outside any package and rejected with
        // X::Attribute::NoPackage. (`unit module`/`unit package` are excluded:
        // the compiler keeps their package context for the rest of the scope.)
        if allow_mainline_capture && starts_unit_class_role_grammar(r) {
            let (after_decl, mut decl) = class::unit_module_stmt(r)?;
            let (tail_rest, tail_stmts) = stmt_list_with_mode(after_decl, false, emit_setline)?;
            match &mut decl {
                Stmt::ClassDecl { body, .. } | Stmt::RoleDecl { body, .. } => *body = tail_stmts,
                _ => {}
            }
            stmts.push(decl);
            return Ok((tail_rest, stmts));
        }
        // Emit source line info for deprecation tracking and error reporting.
        let line = crate::parser::primary::current_line_number(r);
        let line_valid = crate::parser::primary::is_within_original_source(r);
        match statement(r) {
            Ok((r, stmt)) => {
                // In Raku, after a statement-ending block (e.g. `sub f { 3 }`),
                // a semicolon, newline, closing brace, or end-of-input is required
                // before the next statement.  `sub f { 3 } sub g { 3 }` on a
                // single line is a syntax error.
                if is_block_ending_stmt(&stmt) && !has_statement_separator(r) {
                    return Err(PError::fatal(
                        "X::Syntax::Confused: Strange text after block (missing semicolon or comma?)"
                            .to_string(),
                    ));
                }
                if matches!(
                    stmt,
                    Stmt::Package { .. } | Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. }
                ) {
                    saw_compunit_declarator = true;
                }
                if emit_setline && line_valid {
                    stmts.push(Stmt::SetLine(line));
                }
                stmts.push(stmt);
                rest = consume_trailing_comma(r);
                // In Raku, a stray `)` before `;` at statement level is allowed
                // (e.g. `throws-like 'code', Exception, 'msg');`).
                // Consume `)` followed by `;` as a statement terminator.
                rest = consume_stray_close_paren(rest);
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
    // In Raku, a comma can act as a statement separator after block-bearing
    // declarations (e.g. `multi sub foo() { }, multi sub bar() { }`).
    while input.starts_with(';') || input.starts_with(',') {
        input = &input[1..];
        // Also consume whitespace after separators
        if let Ok((r, _)) = ws(input) {
            input = r;
        }
    }
    input
}

/// Consume an optional trailing comma after a statement, used as a statement
/// separator after block-bearing declarations in Raku
/// (e.g. `multi sub foo() { }, multi sub bar() { }`).
fn consume_trailing_comma(input: &str) -> &str {
    if let Ok((after_ws, _)) = ws(input)
        && after_ws.starts_with(',')
    {
        let r = &after_ws[1..];
        if let Ok((after_comma_ws, _)) = ws(r) {
            return after_comma_ws;
        }
        return r;
    }
    input
}

/// Consume a stray `)` followed by `;` at statement level.
/// In Raku, some roast test patterns use `throws-like 'code', Exception, 'msg');`
/// where the `)` before `;` is a stray closing paren that should be ignored.
fn consume_stray_close_paren(input: &str) -> &str {
    if let Ok((after_ws, _)) = ws(input)
        && after_ws.starts_with(')')
    {
        let after_paren = &after_ws[1..];
        if let Ok((after_paren_ws, _)) = ws(after_paren)
            && (after_paren_ws.starts_with(';')
                || after_paren_ws.is_empty()
                || after_paren_ws.starts_with('}'))
        {
            return after_paren_ws;
        }
    }
    input
}
