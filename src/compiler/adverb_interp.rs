//! BEGIN-time evaluation of adverb values in extended identifiers.
//!
//! `src/adverb_name.rs` explains the split: the parser canonicalizes every
//! adverb value it can decide on its own and leaves the rest — `«…»` holding a
//! sigil, and `(…)`/`[…]` holding anything but plain quoted words — wrapped in
//! `adverb_name::INTERP_MARK`, preserving their source spelling. This module
//! finishes them against the compiler's `constant` environment, so
//!
//! ```raku
//! constant $c = 42;
//! my $a:foo<42> = "answer";
//! say $a:foo«$c»;      # answer
//! my $foo:bar<2> = 5;
//! say $foo:bar(1+1);   # 5
//! ```
//!
//! all name the variable their declaration did.
//!
//! Resolution happens at the top of [`Compiler::compile_expr`] /
//! [`Compiler::compile_stmt`] rather than in a whole-AST pre-pass. That keeps
//! the ordering right for free — a `constant` declaration is compiled, and so
//! recorded, before any later statement that mentions it — and needs no
//! recursive rewriter over the ~80 `Expr`/`Stmt` variants, only the handful
//! that carry a variable name as a bare `String`.
//!
//! ## Failure modes
//!
//! Evaluation is BEGIN-time, so only a `constant` is visible; raku rejects a
//! runtime variable there too ("Use of uninitialized value $c", then "Variable
//! '$a:foo<>' is not declared"). An unresolvable `«…»` interpolation is
//! therefore reported as an error rather than silently producing a name that
//! can never match. A `(…)`/`[…]` value that does not evaluate falls back to
//! the parser's literal reading instead: it is the spelling raku uses for a
//! plain word list too (`$today:foo('a','b')`), and mutsu's constant folder
//! does not model every expression raku's BEGIN-time evaluation would.

use crate::adverb_name::{self, INTERP_MARK};
use crate::ast::{Expr, Stmt};

use super::Compiler;

/// A `«…»` interpolation that named a variable no `constant` provides.
pub(super) struct UnresolvedInterp {
    /// The offending variable, as spelled inside the guillemets.
    variable: String,
    /// The extended identifier it appears in, with the sentinels stripped so
    /// the diagnostic shows the source spelling.
    name: String,
}

impl UnresolvedInterp {
    pub(super) fn variable(&self) -> &str {
        &self.variable
    }

    /// The message rakudo's `X::Undeclared` carries, plus why BEGIN time is
    /// the reason a perfectly ordinary `my $c` does not work here.
    pub(super) fn message(&self) -> String {
        format!(
            "Variable '{}' is not declared. An adverb value interpolated into the \
variable name '{}' is evaluated at BEGIN time, so it can only mention a `constant`",
            self.variable, self.name
        )
    }
}

/// A name as the source spelled it: the sentinels are invisible, and mutsu
/// strips a scalar's `$` in the AST, so put it back for the diagnostic.
fn display_name(name: &str) -> String {
    let bare = name.replace(INTERP_MARK, "");
    if bare.starts_with(['$', '@', '%', '&']) {
        bare
    } else {
        format!("${bare}")
    }
}

impl Compiler {
    /// Resolve every marked adverb value in `name`, or report the first
    /// interpolation that named no in-scope `constant`.
    pub(super) fn resolve_adverb_name(&self, name: &str) -> Result<String, UnresolvedInterp> {
        let mut out = String::with_capacity(name.len());
        let mut rest = name;
        while let Some(open) = rest.find(INTERP_MARK) {
            out.push_str(&rest[..open]);
            let after = &rest[open + INTERP_MARK.len_utf8()..];
            let Some(close) = after.find(INTERP_MARK) else {
                // Unterminated sentinel: not something the parser can produce.
                // Leave the remainder alone rather than inventing a name.
                out.push_str(after);
                return Ok(out);
            };
            let evaluated = self
                .eval_adverb_value(&after[..close])
                .map_err(|variable| UnresolvedInterp {
                    variable,
                    name: display_name(name),
                })?;
            // A constant whose own value contains the sentinel would otherwise
            // leave the result still "needing interpolation", and the
            // compile_expr/compile_stmt hooks would recurse forever.
            out.push_str(&evaluated.replace(INTERP_MARK, ""));
            rest = &after[close + INTERP_MARK.len_utf8()..];
        }
        out.push_str(rest);
        Ok(out)
    }

    /// Evaluate one wrapped adverb value (`«$c»` or `(1+1)`) to its canonical
    /// `<...>` spelling.
    fn eval_adverb_value(&self, spelling: &str) -> Result<String, String> {
        let mut chars = spelling.chars();
        let open = chars.next().unwrap_or('(');
        let close_len = chars.next_back().map_or(0, char::len_utf8);
        let content = &spelling[open.len_utf8()..spelling.len() - close_len];
        if open == '\u{00AB}' {
            let interpolated = self.interpolate_guillemets(content)?;
            return Ok(format!("<{}>", adverb_name::normalize_words(&interpolated)));
        }
        Ok(format!("<{}>", self.eval_expression_list(content)))
    }

    /// `«…»` is `qqw`: interpolate first, then split on whitespace. Only the
    /// simple `$name` / `@name` / `%name` / `&name` spelling is supported —
    /// that is what the documentation shows, and anything richer would need a
    /// BEGIN-time evaluator mutsu does not have.
    fn interpolate_guillemets(&self, content: &str) -> Result<String, String> {
        let mut out = String::with_capacity(content.len());
        let mut i = 0;
        while i < content.len() {
            let c = content[i..]
                .chars()
                .next()
                .expect("index is a char boundary");
            if !matches!(c, '$' | '@' | '%' | '&') {
                out.push(c);
                i += c.len_utf8();
                continue;
            }
            let Some(ident_len) = identifier_len(&content[i + 1..]) else {
                out.push(c);
                i += 1;
                continue;
            };
            let ident = &content[i + 1..i + 1 + ident_len];
            // mutsu strips the sigil from a variable's AST name, so a
            // `constant $c` is recorded under the bare `c`.
            let Some(value) = self.compile_time_constant(ident) else {
                return Err(format!("{c}{ident}"));
            };
            out.push_str(&value.to_string_value());
            i += 1 + ident_len;
        }
        Ok(out)
    }

    /// `(…)`/`[…]` holds an expression list: evaluate each item at BEGIN time
    /// and join the stringified results, which is what makes `$a:foo(1+1)` and
    /// `$a:foo<2>` the same name. Falls back to the parser's literal reading
    /// when the list is not compile-time constant.
    fn eval_expression_list(&self, content: &str) -> String {
        self.try_eval_expression_list(content)
            .unwrap_or_else(|| adverb_name::literal_paren_words(content))
    }

    fn try_eval_expression_list(&self, content: &str) -> Option<String> {
        let (stmts, _) = crate::parse_dispatch::parse_fragment(content).ok()?;
        let expr = match stmts.as_slice() {
            [Stmt::Expr(e)] => e,
            [Stmt::SetLine(_), Stmt::Expr(e)] => e,
            _ => return None,
        };
        let items: Vec<&Expr> = match expr {
            // A bare comma list (`1,2`) parses as an array literal.
            Expr::ArrayLiteral(items) => items.iter().collect(),
            single => vec![single],
        };
        let mut words = Vec::with_capacity(items.len());
        for item in items {
            words.push(self.const_operand_begin_time(item)?.to_string_value());
        }
        Some(adverb_name::normalize_words(&words.join(" ")))
    }
}

/// Length of the Raku identifier at the start of `s`, or `None` if it does not
/// start with one. Accepts the `-`/`'` infix Raku allows between name
/// characters, and `::` package separators.
fn identifier_len(s: &str) -> Option<usize> {
    let first = s.chars().next()?;
    if !(first.is_alphabetic() || first == '_') {
        return None;
    }
    let mut end = first.len_utf8();
    loop {
        let rest = &s[end..];
        let Some(c) = rest.chars().next() else { break };
        if c.is_alphanumeric() || c == '_' {
            end += c.len_utf8();
            continue;
        }
        // `-`/`'` and `::` only continue the identifier when a name character
        // follows; a trailing one belongs to the surrounding text.
        let joiner = match c {
            '-' | '\'' => 1,
            ':' if rest.starts_with("::") => 2,
            _ => break,
        };
        match s[end + joiner..].chars().next() {
            Some(n) if n.is_alphanumeric() || n == '_' => end += joiner + n.len_utf8(),
            _ => break,
        }
    }
    Some(end)
}

/// True when `expr` is a variable reference whose name still needs BEGIN-time
/// adverb-value evaluation.
pub(super) fn expr_needs_interp(expr: &Expr) -> bool {
    expr_name(expr).is_some_and(adverb_name::needs_interp)
}

/// The variable name `expr` carries, if any.
fn expr_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Var(n)
        | Expr::CaptureVar(n)
        | Expr::ArrayVar(n)
        | Expr::HashVar(n)
        | Expr::CodeVar(n)
        | Expr::AssignExpr { name: n, .. } => Some(n),
        _ => None,
    }
}

/// True when `stmt` declares or assigns a variable whose name still needs
/// BEGIN-time adverb-value evaluation.
pub(super) fn stmt_needs_interp(stmt: &Stmt) -> bool {
    stmt_name(stmt).is_some_and(adverb_name::needs_interp)
}

/// The variable name `stmt` carries, if any.
fn stmt_name(stmt: &Stmt) -> Option<&str> {
    match stmt {
        Stmt::VarDecl { name, .. }
        | Stmt::Assign { name, .. }
        | Stmt::MarkReadonly(name, _)
        | Stmt::MarkBoundContainer(name)
        | Stmt::MarkSigillessReadonly(name)
        | Stmt::MarkSigilless(name) => Some(name),
        _ => None,
    }
}

impl Compiler {
    /// `expr` with its variable name resolved.
    pub(super) fn resolve_expr_name(&self, expr: &Expr) -> Result<Expr, UnresolvedInterp> {
        let name = self.resolve_adverb_name(expr_name(expr).unwrap_or_default())?;
        Ok(match expr {
            Expr::Var(_) => Expr::Var(name),
            Expr::CaptureVar(_) => Expr::CaptureVar(name),
            Expr::ArrayVar(_) => Expr::ArrayVar(name),
            Expr::HashVar(_) => Expr::HashVar(name),
            Expr::CodeVar(_) => Expr::CodeVar(name),
            Expr::AssignExpr { expr, is_bind, .. } => Expr::AssignExpr {
                name,
                expr: expr.clone(),
                is_bind: *is_bind,
            },
            other => other.clone(),
        })
    }

    /// `stmt` with its variable name resolved.
    pub(super) fn resolve_stmt_name(&self, stmt: &Stmt) -> Result<Stmt, UnresolvedInterp> {
        let name = self.resolve_adverb_name(stmt_name(stmt).unwrap_or_default())?;
        let mut resolved = stmt.clone();
        match &mut resolved {
            Stmt::VarDecl { name: n, .. }
            | Stmt::Assign { name: n, .. }
            | Stmt::MarkReadonly(n, _)
            | Stmt::MarkBoundContainer(n)
            | Stmt::MarkSigillessReadonly(n)
            | Stmt::MarkSigilless(n) => *n = name,
            _ => {}
        }
        Ok(resolved)
    }

    /// Report an adverb value that mentions something no `constant` provides.
    /// raku fails to compile such a program; mutsu has no BEGIN-time abort, so
    /// the unit compiles and throws when control reaches the name.
    pub(super) fn emit_adverb_name_error(&mut self, err: &UnresolvedInterp) {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "name".to_string(),
            crate::value::Value::str_from(err.variable()),
        );
        attrs.insert(
            "symbol".to_string(),
            crate::value::Value::str_from(err.variable()),
        );
        attrs.insert(
            "suggestions".to_string(),
            crate::value::Value::array(vec![]),
        );
        attrs.insert(
            "message".to_string(),
            crate::value::Value::str(err.message()),
        );
        let value = crate::value::Value::make_instance(
            crate::symbol::Symbol::intern("X::Undeclared"),
            attrs,
        );
        let idx = self.code.add_constant(value);
        self.code.emit(crate::opcode::OpCode::LoadConst(idx));
        self.code
            .emit(crate::opcode::OpCode::Die { user_throw: false });
    }
}

#[cfg(test)]
mod tests {
    use super::identifier_len;

    #[test]
    fn identifier_lengths() {
        assert_eq!(identifier_len("c"), Some(1));
        assert_eq!(identifier_len("c "), Some(1));
        assert_eq!(identifier_len("foo-bar rest"), Some(7));
        assert_eq!(identifier_len("foo- rest"), Some(3));
        assert_eq!(identifier_len("Foo::Bar x"), Some(8));
        assert_eq!(identifier_len("a1b2"), Some(4));
        assert_eq!(identifier_len("1abc"), None);
        assert_eq!(identifier_len(""), None);
        assert_eq!(identifier_len("-x"), None);
    }
}
