//! `<::(EXPR)>`: a `<subrule>` call whose rule name is computed per call
//! (ADR-0135 §8, Slice E). The name is evaluated where the cursor reaches the
//! call, then the call is resolved and run as `<name>` would be: a frame for a
//! rule or a proto, a grammar method on the calling frame's cursor, or a
//! builtin.

use crate::ast::{Expr, Stmt};
use crate::runtime::Interpreter;
use crate::runtime::regex_types::{NamedAtom, RegexCaptures};
use crate::token_kind::TokenKind;
use crate::value::ValueView;

impl Interpreter {
    /// The rule `<::(EXPR)>` calls, its name computed by running `EXPR` with
    /// the captures `caps` of the calling frame: the `<name>` the call is then
    /// resolved and run as. `None` when `EXPR` fails to evaluate (no match).
    // Cost: one run of `EXPR`.
    pub(super) fn rx_symbolic_called(
        &mut self,
        name: &NamedAtom,
        caps: &RegexCaptures,
    ) -> Option<NamedAtom> {
        let expr = name.spec().arg_exprs.first()?;
        let constant = self.symbolic_name_is_constant(expr);
        let value = self.eval_regex_expr_value(expr, caps)?;
        let resolved = value.to_string_value();
        // A name written out in the regex (`<::("x")>`) is a call of that rule,
        // filed under its name. A computed one (`<::($n)>`) is looked up on the
        // cursor at run time and files nothing, like `<.x>`: rakudo gives
        // `<::($n)>` no capture of its own (an alias still captures it).
        Some(NamedAtom::from(if constant || resolved.starts_with('.') {
            resolved
        } else {
            format!(".{resolved}")
        }))
    }

    /// The rule name a symbolic call (`<::("alpha")>`) spells out, when its
    /// argument is a single plain string literal; `None` for anything that has
    /// to be evaluated to be known. A static answer, for the analyses that run
    /// before a match (which names a quantifier turns into lists).
    // Cost: O(n), n = the length of the argument text.
    pub(in crate::runtime) fn symbolic_call_written_name(
        spec: &crate::runtime::regex::regex_helpers::NamedRegexLookupSpec,
    ) -> Option<String> {
        let arg = spec.arg_exprs.first()?.trim();
        let quote = arg.chars().next().filter(|c| matches!(c, '"' | '\''))?;
        let body = arg.strip_prefix(quote)?.strip_suffix(quote)?;
        let plain = !body.is_empty()
            && !body.contains(['"', '\'', '\\', '$', '@', '%', '&', '{', '}', '\n']);
        plain.then(|| body.to_string())
    }

    /// Whether the name expression of a symbolic call is known where the regex
    /// is written: a string literal, or a `~` of them (rakudo folds those, so
    /// `<::("a" ~ "b")>` names `ab` outright). Anything that has to run to find
    /// the name — a variable, an interpolated string, a method call — is not.
    // Cost: O(1) for a bare `$var` (no parse); otherwise one memoized parse of the
    // argument text, then O(e), e = the nodes of the name expression.
    pub(in crate::runtime) fn symbolic_name_is_constant(&self, expr_src: &str) -> bool {
        let trimmed = expr_src.trim();
        if trimmed.starts_with('$') {
            return false;
        }
        let Some((stmts, _)) = self.parse_regex_code_cached_with_id(&format!("({trimmed});"))
        else {
            return false;
        };
        let mut root = None;
        for stmt in stmts.iter() {
            match stmt {
                Stmt::SetLine(..) => {}
                Stmt::Expr(expr) if root.is_none() => root = Some(expr),
                _ => return false,
            }
        }
        let Some(root) = root else {
            return false;
        };
        let mut pending = vec![root];
        while let Some(expr) = pending.pop() {
            match expr {
                Expr::Grouped(inner) => pending.push(inner),
                Expr::Literal(v) if matches!(v.view(), ValueView::Str(_)) => {}
                Expr::Binary {
                    left,
                    op: TokenKind::Tilde,
                    right, ..
                } => {
                    pending.push(left);
                    pending.push(right);
                }
                _ => return false,
            }
        }
        true
    }
}
