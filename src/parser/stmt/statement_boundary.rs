use super::*;

/// Check if a statement ends with a block (closing `}`).
/// These "statement-ending blocks" can omit the semicolon only if followed
/// by a newline, semicolon, closing brace, comma, or end-of-input.
pub(super) fn is_block_ending_stmt(stmt: &Stmt, consumed: &str) -> bool {
    match stmt {
        // A genuinely empty body is still a block. The consumed source can
        // establish that fact where an empty statement vector cannot.
        Stmt::SubDecl { body, .. } => !body.is_empty() || consumed.ends_with('}'),
        // `when`/`default` block statements take a `{...}` body and, unlike
        // `if`/`for`/etc., are not followed by any same-line continuation
        // keyword (no `else`/`elsif`) and do not consume trailing statement
        // modifiers. So `when 1 { } when 2 { }` on one line (no `;`) is a
        // "Strange text after block" error in Raku, matching `given $x { ... }`.
        Stmt::When { .. } | Stmt::Default(_) => true,
        // The same, kept in its RakuAST shape by a `.AST` parse.
        Stmt::Ast(node) => matches!(
            node.as_ref(),
            crate::ast::parsed::ParsedStmt::When { .. }
                | crate::ast::parsed::ParsedStmt::Default(_)
        ),
        // A direct method declaration ends with its body. Lexical and
        // package-qualified methods are excluded because `my_decl` parses their
        // trailing statement modifier before returning them here. The `is_my`
        // flag also marks direct submethods, which the declaration parser handles
        // without `my_decl`.
        Stmt::MethodDecl {
            is_my: false,
            is_our: false,
            ..
        }
        | Stmt::MethodDecl {
            is_submethod: true,
            is_our: false,
            ..
        } => true,
        // Stmt::Block, Stmt::If, Stmt::For, ClassDecl, RoleDecl, Package, etc. are
        // also excluded for similar reasons -- they may go through paths that consume
        // trailing separators before returning to stmt_list_with_mode.
        _ => false,
    }
}

/// Check a proto body's source boundary before its AST loses body syntax.
pub(super) fn require_proto_body_boundary(rest: &str) -> Result<(), PError> {
    if has_statement_separator(rest) || rest.starts_with(['(', '[', '{', ')', ']']) {
        return Ok(());
    }
    let (next, _) = ws(rest)?;
    if next.starts_with('.') || super::modifier::is_stmt_modifier_keyword(next) {
        return Ok(());
    }
    Err(missing_block_separator())
}

pub(super) fn missing_block_separator() -> PError {
    PError::fatal(
        "X::Syntax::Confused: Strange text after block (missing semicolon or comma?)".to_string(),
    )
}

/// A routine declaration can continue with a postfix or statement modifier.
pub(super) fn routine_decl_continuation_follows(stmt: &Stmt, rest: &str) -> bool {
    matches!(
        stmt,
        Stmt::SubDecl { .. }
            | Stmt::ProtoDecl { .. }
            | Stmt::ProtoToken { .. }
            | Stmt::MethodDecl {
                is_my: false,
                is_our: false,
                ..
            }
            | Stmt::MethodDecl {
                is_submethod: true,
                is_our: false,
                ..
            }
    ) && (rest.starts_with('.') || super::modifier::is_stmt_modifier_keyword(rest))
}

/// Check that the remaining input has a proper statement separator
/// (newline, semicolon, comma, closing brace, or end-of-input)
/// before any non-whitespace code on the same line.
pub(super) fn has_statement_separator(mut rest: &str) -> bool {
    while let Some(ch) = rest.chars().next() {
        if ch == '#' {
            // An embedded comment, including its internal newlines, is not
            // a statement separator. A line comment consumes the line.
            if let Some(after) = crate::parser::helpers::skip_embedded_comment(rest)
                .or_else(|| crate::parser::helpers::skip_declarator_doc_comment(rest))
            {
                rest = after;
                continue;
            }
            return true;
        }
        match ch {
            // Valid separators: newline, semicolon, closing brace, comma,
            // or `#` (start of line comment, effectively end-of-statement)
            '\n' | '\r' | '\u{000b}' | '\u{000c}' | '\u{0085}' | '\u{2028}' | '\u{2029}' | ';'
            | '}' | ',' => return true,
            // Horizontal Unicode whitespace does not end the statement.
            ch if ch.is_whitespace() => rest = &rest[ch.len_utf8()..],
            // Any other non-whitespace on the same line -> missing separator
            _ => return false,
        }
    }
    // End of input is a valid separator
    true
}
